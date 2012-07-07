#!/usr/bin/perl
package Rolebot::Bot;
use strict; use warnings;
#use feature qw(switch state);
use v5.10;
use utf8;

use Rolebot::Config;
use Rolebot::Util::LevenshteinDamerau 'distance';
use POE::Session;
use POE::Kernel;
use List::MoreUtils qw(uniq);
use File::chdir;
use File::Spec 'rel2abs';
use File::Basename qw(fileparse dirname);
use LWP::UserAgent ();
use HTTP::Request ();
use Tie::Persistent;
use Data::Dumper 'Dumper';
use Storable 'dclone';
$Storable::Deparse = 'true';
$Storable::Eval = 'true';
BEGIN {
    eval {
        require Math::Random::MT::Perl; Math::Random::MT::Perl->import('rand');
    };
    warn "Optional module Math::Random::MT::Perl not found.\n" if $@;
}

use base qw(Bot::BasicBot);
our @EXPORT = qw( cmd_names plugin_names trim parse_subcommand cap get_url command filter);

#absolute path to the bot's directory.
our $bot_dir = File::Spec->rel2abs(dirname $0); 


our $global_filters = {};
our $global_cmds = {};
our $current_bot;

#command-specific data (soon to be removed)
my %dsystems;
my $default_roller = 'owod';

#whois data
my %whois_pending;

sub is_super_admin($_) {
    my ($self, $nick) = @_;
    return $self->is_registered($nick) && grep {lc $_ eq lc $nick} @Rolebot::Config::super_admins;
}
sub is_admin(_) {
    my ($self, $nick) = @_;
    return $self->is_registered($nick) && grep {lc $_ eq lc $nick} $self->admins;
}

sub admins {
    my ($self, @new) = @_;
    my $admins = $self->{admins} //= [];
    if (@new) {
        my %seen;
        @$admins = grep {my$x=$_;!($seen{$_}++ || grep {$_ eq $x} @Rolebot::Config::super_admins)} @new;
    }
    return @Rolebot::Config::super_admins, @$admins;
}

sub registered_nicks {
    my ($self, @new) = @_;
    my $registered = $self->{registered_nicks} //= {};
    %$registered = @new if @new;
    return $registered;
}

sub is_registered($_) {
    my ($self, $nick) = @_;
    return $self->pocoirc->is_user_mode_set('r') if $self->nick eq $nick;
    $nick = lc $nick;
    $self->whois($nick) unless $self->registered_nicks->{$nick};
    return $self->registered_nicks->{$nick} //= 0;
}

sub whois {
    my ($self, $nick) = @_;
    unless ($whois_pending{$nick}) {
        $whois_pending{$nick} = 1;
        $self->SUPER::whois($nick);
    }
}

sub cmd_names {
    my ($self) = @_;
    my %cmds = %{$self? $self->{cmds} : $global_cmds};
    return grep {defined} keys %cmds;
}

sub plugin_names { 
    my ($self) = @_;
    my $plugins = $self->{plugins};
    return grep {defined} keys %$plugins ;
}

sub system_names { return grep {defined} keys %dsystems }

sub state {
    my ($self, $plugin) = @_;
    my ($_, undef) = caller;
    /Rolebot::Plugins::(\w+)/;
    return unless $1;
    my $plugins = $self->{plugins};
    my $state = $plugins->{$1}->{state};
    unless ($plugins->{$1}->{state}) {
        tie %$state, 'Tie::Persistent', "$bot_dir/state/$1", $Rolebot::Config::state_file_mode;
    }
    return $state;
}

sub get_state {
    my ($self, $plugin) = @_;
    return unless defined $plugin;
    if (my $p = $self->{plugins}->{$plugin}) {
        return $p->{state};
    }
    return;
}

sub trim {
    state $parse_arg_string = qr/^\s*(.*?)\s*$/p;
    shift  =~ $parse_arg_string;
    return $1
}

sub parse_subcommand {
    state $parse_subcommand = qr/^(\S*)(?: +(.*))? *$/p;
    return shift =~ $parse_subcommand;
}

sub cap(\$;$) {
    my ($s, $n, $l) = @_;
    my @lines = map {substr $_, 0, $l // $Rolebot::Config::line_cap} split /\n+/, $$s;
    return (defined wantarray? my $_ : $$s) = join "\n", (splice @lines, 0, $n // $Rolebot::Config::max_lines);
}

sub get_url(_) {
    state $ua = LWP::UserAgent->new(agent => $Rolebot::Config::useragent);
    return $ua->request(HTTP::Request->new('GET', shift));
}

sub call_plugin_with_callback {
    my ($self, $plugin, $func, $callback, @args) = @_;
    if (my $info = $self->{plugins}->{$plugin}) {
        local $current_bot = $self;
        my @r = eval {
            no strict 'refs';
            &{$info->{mod} . "::$func"}($self, @args);
        };
        my $mod = $info->{mod};
        warn $@ if $@ && $@ !~ /^Undefined subroutine &${mod}::$func called/;
        @r = $callback->(@r);
        my $state = $info->{state};
        (tied %$state)->sync if $state;
        return @r;
    }
}

sub call_plugin_no_say {
    my ($self, $plugin, $func, @args) = @_;
    $self->call_plugin_with_callback($plugin, $func, sub {}, @args);
}

sub call_plugin {
    my ($self, $plugin, $func, $a, @args) = @_;
    $self->call_plugin_with_callback($plugin, $func, 
                                     sub {
                                         $self->say(%$a, @_) if @_;
                                     }, $a, @args);
}

sub call_plugins_with_callback {
    my ($self, @args) = @_;
    map {$self->call_plugin_with_callback($_,@args)} $self->plugin_names;
}

sub call_plugins_no_say {
    my ($self, @args) = @_;
    map {$self->call_plugin_no_say($_, @args)} $self->plugin_names;
}

sub call_plugins {
    my ($self, @args) = @_;
    $self->call_plugin($_, @args) for $self->plugin_names;
}

sub load_plugins {
    my ($self, @plugin_names) = @_;
    my $success = 1;
    my $plugins = $self->{plugins};
    for my $name (@plugin_names) {
        $name =~ s/\.pm$//;
        my $path = "$bot_dir/Plugins/$name";
        my $mod;
        if (-d $path && $path !~ /.disabled$/) {
            $mod  = "Rolebot::Plugins::${name}::Load";
            $path = File::Spec->catfile($path, "Load.pm");
        }
        elsif (-f "$path.pm") {
            $path .= ".pm";
            $mod  = "Rolebot::Plugins::$name";
        }
        else { $success = 0; next }
        $path = File::Spec->rel2abs($path);
        $self->unload_plugins($name) if defined $plugins->{$name};
        $plugins->{$name} = {mod => $mod, path => $path};
        print "Loading $name...\n";
        local $current_bot = $self;
        do $path;
        if ($@) {
            delete $plugins->{$name};
            warn $@;
            $success = 0;
        }
        else {
            $self->call_plugin_no_say($name, 'load');
            $self->call_plugins_no_say('plugin_loaded', $name);
        }
    }
    return $success;
}

sub unload_plugins {
    my ($self, @plugin_names) = @_;
    my $success = 1;
    my $plugins = $self->{plugins};
    for my $plugin_name (@plugin_names) {
        $self->call_plugin_no_say($plugin_name, 'unload');
        if ($plugins->{$plugin_name}) {
            for my $h ($self->{cmds}, $self->{filters}, \%dsystems) {
                for (keys %$h) {
                    delete $h->{$_} if $h->{$_}->{mod} =~ /^Rolebot::Plugins::$plugin_name/
                }
            }
            delete $plugins->{$plugin_name};
            $self->call_plugins_no_say('plugin_unloaded', $plugin_name);
        }
        else { $success = 0;}
    }
    return $success;
}

sub init {
    my ($self) = @_;
    $self->{cmds} = dclone $global_cmds;
    $self->{filters} = dclone $global_filters;
    $self->{plugins} = {};
    my $admins = $self->{admins} = [];
    tie @$admins, 'Tie::Persistent', $Rolebot::Config::admins_file, 'rw';
    (tied @$admins)->autosync(1);
    my $state_dir = "$bot_dir/state";
    qx(mkdir $state_dir) unless -d $state_dir;
    if (opendir my $d, "Plugins") {
        $self->load_plugins(grep !/(^\.\.?)|(.*~$)$/, readdir $d);
    }
    else {
        warn "Unable to load plugins directory.";
    }
    $self->{cmds}->{roll} = $dsystems{$default_roller};
    $self->call_plugins_no_say('init');
    return 1;
}

sub chanjoin {
    my ($self, $a) = @_;
    $self->is_registered($a->{who}) unless $self->nick eq $a->{who};
    $self->call_plugins('chanjoin', $a);
    return;
}

sub chanpart {
    my ($self, $a) = @_;
    $self->call_plugins('chanpart', $a);
    return;
}

sub got_names {
    my ($self, $a) = @_;
    #$self->whois($_) for keys %{$a->{names}};
    $self->call_plugins_no_say('got_names', $a);
}

sub nick_change {
    my ($self, $nick, $newnick) = @_;
    $self->registered_nicks->{$nick} = undef;
    $self->registered_nicks->{$newnick} = undef;
    $self->call_plugins_no_say('nick_change', $nick, $newnick);
}

sub topic {
    my ($self, $a) = @_;
    $self->call_plugins('topic', $a);
}

sub kicked {
    my ($self, $a) = @_;
    $self->registered_nicks->{$a->{who}} = undef;
    $self->call_plugins_no_say('kicked', $a);
}

sub connected {
    my ($self) = @_;
    $self->call_plugins_no_say('connected');
}

sub userquit {
    my ($self, $a) = @_;
    $self->registered_nicks->{$a->{who}} = undef;
    $self->call_plugins('userquit', $a);
    return;
}

sub whois_state {
    my ($self, $data) = @_[OBJECT, ARG0];
    my $nick = lc $data->{nick};
    my $i = $data->{identified} // ($data->{modes}//'') =~ /r/;
    $self->registered_nicks->{lc $nick} = $i;
    $whois_pending{$nick} = 0;
    $self->call_plugins_no_say('whois', $data);
}

sub say {
    return Bot::BasicBot::say(@_) unless ref $_[0];
    my $self = shift;
    my $args;
    if (ref $_[0]) {
        $args = shift
    }
    else {
        $args = {@_};
    }
    $self->call_plugins_no_say("msg_filter", $args);
    return $self->SUPER::say($args);
}


sub said {
    my ($self,$args) = @_;
    my $nick = $args->{who};
    return if $self->ignore_nick($nick);
    unless (defined $self->registered_nicks->{lc $nick}) {
        $self->whois($nick);
        $self->registered_nicks->{lc $nick} = 0;
    }
    $self->call_plugins('said', $args);
    my $filters = $self->{filters};
    for(keys %$filters) {
        my %default_msg_info = (who=>$nick, channel=>$args->{channel});
        if (my @match = $args->{body} =~ $_) {
            my @result;
            my %msg_info;
            my $filter = $filters->{$_};
            eval {
                @result = $filter->{code}->($self, $args, @match);
            };
            my $state = $self->get_state($filter->{plugin});
            (tied %$state)->sync if $state;
            warn $@ if $@;
            %msg_info = @result? @result : ();
            if ($msg_info{body}) {
                cap $msg_info{body} unless $args->{channel} eq "msg";
                $self->say(%default_msg_info, %msg_info);
            }
        }
    }
}

sub emoted {
    my ($self, $a) = @_;
    $self->whois($a->{who}) unless defined $self->registered_nicks->{lc $a->{who}};
    $self->call_plugins('emoted', $a);
}

sub noticed {
    my ($self, $a) = @_;
    $self->call_plugins('noticed', $a);
}

sub help {
    my ($self, $a) = @_;
    $a->{body} = '';
    my %result = $self->{cmds}->{help}->{code}->($self, $a);
    return $result{body};
}

sub filter {
    my ($pattern, $code) = @_;
    my $filters = $current_bot? $current_bot->{filters} : $global_filters;
    warn "Filter pattern <<$pattern>> already exists and is being replaced by " . caller if defined $filters->{$pattern};
    my ($module, $filepath) = caller;
    $module =~ /Rolebot::Plugins::(\w+)/;
    $filters->{$pattern} = {code => $code, path => $filepath, mod => $module,
                            plugin => $1};
    return 1;
}

my $parse_command     = qr/^[\Q$Rolebot::Config::cmd_prefixes\E]([^ ]+)(?:\s+(.*)\s*)?$/p;

filter $parse_command,
sub {
    my ($self, $args, $cmd_name, $cmd_args) = @_;
    my $cmds = $self->{cmds};
    my $cmd;
    unless ($cmd = $cmds->{$cmd_name}) {
        my @abbrevs;
        my %close;
        for(keys %$cmds) {
            push @abbrevs, $_ if /^\Q$cmd_name\E/;
            if (3 > (my $d = distance($cmd_name, $_))) {
                $close{$_} = $d ;
            }
        }
        if (@abbrevs == 1) { $cmd = $cmds->{$abbrevs[0]}; }
        elsif (keys %close == 1) { $cmd = $cmds->{(keys %close)[0]}; }
        elsif (@abbrevs || keys %close)  {
            @abbrevs = sort {length $a <=> length $b} @abbrevs;
            my @close = sort {$close{$a} <=> $close{$b}} keys %close;
            my $matches = join(' ' , uniq(@abbrevs, @close));
            return (body => "Perhaps you meant: $matches");
        }
        else  { return }
    }
    $args->{body} = $cmd_args // '';
    $args->{help} = $cmd->{help};
    my @result = $cmd->{code}->($self, $args);
    my $state = $self->get_state($cmd->{plugin});
    (tied %$state)->sync if $state;
    return @result;
};

sub command {
    my ($cmds, $name, $cat, $doc, $cmd);
    if    ($#_==1) { ($name, $cmd) = @_ }
    elsif ($#_==2) { ($name, $doc, $cmd) = @_ }
    else           { ($name, $cat, $doc, $cmd) = @_ }
    $cmds = defined $current_bot? $current_bot->{cmds} : $global_cmds;
    $cat //= $Rolebot::Config::default_help_category;
    warn qq(Command "$name" already exists and is being replaced by ) . caller if defined $cmds->{$name};
    my ($mod, $path) = caller;
    $mod =~ /Rolebot::Plugins::(\w+)/;
    $cmds->{$name} = {code => $cmd, help => $doc, cat => $cat,
                      mod => $mod, path => $path, plugin => $1};
    return 1;
}

command load => Admin => "Usage: load <plugin> -- loads or reloads a plugin.",
sub {
    my ($self, $a) = @_;
    my $plugin_name = trim $a->{body};
    my $plugins = $self->{plugins};
    return (body => "Insufficient privileges.") unless $self->is_admin($a->{who});
    return (body => $a->{help} . "\nPlugins: @{[keys %$plugins]}") unless $plugin_name;
    return (body => "Done.") if $self->load_plugins($plugin_name);
    return (body => "$plugin_name failed to load.");
};

command unload => Admin => "Usage unload <plugin> -- unloads a plugin.",
sub {
    my ($self, $a) = @_;
    my $plugin_name = trim $a->{body};
    my $plugins = $self->{plugins};
    return (body => "Insufficient privileges.") unless $self->is_admin($a->{who});
    return (body => $a->{help} . "\nPlugins: @{[keys %$plugins]}") unless $plugin_name;
    return (body => "Done.") if $self->unload_plugins($plugin_name);
    return (body => "No plugin named $plugin_name.");
};

command help =>
sub {
    my ($self, $arg) = @_;
    my $cmds = $self->{cmds};
    my $query = trim $arg->{body};
    my $msg;
    #handle specific queries
    if ($query) {
        my $cmd = $cmds->{$query};
        $msg = $cmd->{help} if $cmd;
        unless($msg) {
            my @cmds = sort grep {lc $cmds->{$_}->{cat} eq lc $query} $self->cmd_names;
            $msg = @cmds? "$query commands: " . join(', ', @cmds) : 'No help entry found.';
        }
    }
    #handle default help message
    else {
        my $sep = "\x{3}2|\x{F}";

        #Generate category names
        my %cats;
        $cats{$_->{cat}}++ for values %$cmds;
        my $is_admin = $self->is_admin($arg->{who});
        my @catnames =  sort grep {$_ && ($is_admin || $_ ne 'Admin')} keys %cats;
        my $or_cat = @catnames? " or category" : '';

        #Display command prefixes
        my @prefixes = split //, $Rolebot::Config::cmd_prefixes;
        $prefixes[-1] = "or $prefixes[-1]" if @prefixes > 1;
        $msg = "Commands begin with " . join(' ', @prefixes);

        #Display help usage info
        $msg .= " $sep Use help <name> to get more help on a specific command$or_cat.";

        #Display command categories
        $msg  .= " $sep Categories: " . join(', ', @catnames) if @catnames;

        #Display uncategorized commands
        my @misccmds = sort grep {!$cmds->{$_}->{cat} && $_ ne 'help'} $self->cmd_names;
        $msg  .= " $sep Misc. commands: " . join(', ', @misccmds) if @misccmds;
    }
    return (body => $msg);
};


command system => RP => 'Usage: system [<system name>] -- switches the dice roller to another system',
sub {
    my ($self, $body) = (shift, shift->{body});
    $body = trim $body;
    return (body=>'Dice systems: ' . join (', ', sort $self->system_names)) unless $body;
    my $msg;
    if (my $cmd = $dsystems{lc $body}) {
        $self->{cmds}->{roll} = $cmd;
        $msg = 'Done';
    }
    else { $msg = "No system named $body." }
    return (body=>$msg);
};

sub diceroller($&) {
    my ($name, $doc, $cmd);
    if    ($#_==1) { ($name, $cmd) = @_ }
    elsif ($#_==2) { ($name, $doc, $cmd) = @_ }
    my ($module, $path) = caller;
    $dsystems{$name} = {code => $cmd, help => $doc, cat => 'RP',
                        mod => $module, path => $path} if $cmd;
    1;
}

diceroller owod =>
sub {
    my ($self, $a) = @_;
    my ($n, $d) = $a->{body} =~ /^ *(\d+)(?: +(\d*))? *$/;
    $d //= 6;
    $n //= 0;
    my $msg;
    $msg = 'Invalid difficulty' if $d > 10 || $d < 2;
    $msg = 'Invalid dice count' if $n > 20 || $n < 1;
    $msg = 'Usage: roll <number> [<diff>]' unless $n;
    return (body=>$msg) if $msg;
    my $s = 0;
    my $cant_botch = 0;
    my @rlist = ();
    for(my $i=$n; $i > 0; $i--) {
        my $roll = int(rand(10))+1;
        if ($roll >= $d) {
            $s++;
            $cant_botch=1;
            $i++ if $roll==10; 
        }
        elsif ($roll==1) {
            $s--;
        }
        push @rlist, $roll;
    }
    $s = $cant_botch? 0 : -1 if $s <= -1;
    $msg = "$s success" . ($s==1? '' : 'es') . " (@rlist)";
    return (body=>$msg);
};

diceroller shadowrun =>
sub {
    my ($self, $a) = @_;
    my ($n, $t) = $a->{body} =~ /^ *(\d+)(?: +(\d*))? *$/;
    $t //= 4;    
    my $msg;
    $msg = "Invalid target number" if $t > 6 || $t < 1;
    $msg = "Invalid dice count" if $n > 15 || $n < 1;
    $msg = "roll <number of dice> [<target number>]" unless defined($n);
    return (body => $msg) if $msg;
    my $h = 0;
    my @rlist = ();
    for (my $i = $n; $i > 0; $i--) {
        my $roll = int(rand(6))+1;
        if ($roll >= $t) {
            $h++;
        }
        push @rlist, $roll;
    }
    $msg = "$h hit" . ($h==1? '': 's') . " (@rlist)";
    return (body=>$msg);
};

sub run {
    my $self = shift;

    # create the callbacks to the object states
    POE::Session->create(
        object_states => [
            $self => {
                _start => "start_state",
                die    => "die_state",

                irc_001          => "irc_001_state",
                irc_msg          => "irc_said_state",
                irc_public       => "irc_said_state",
                irc_ctcp_action  => "irc_emoted_state",
                irc_notice       => "irc_noticed_state",

                irc_disconnected => "irc_disconnected_state",
                irc_error        => "irc_error_state",

                irc_join         => "irc_chanjoin_state",
                irc_part         => "irc_chanpart_state",
                irc_kick         => "irc_kicked_state",
                irc_nick         => "irc_nick_state",
                irc_quit         => "irc_quit_state",

                fork_close       => "fork_close_state",
                fork_error       => "fork_error_state",

                irc_366          => "names_done_state",

                irc_332          => "topic_raw_state",
                irc_topic        => "topic_state",

                irc_shutdown     => "shutdown_state",

                irc_whois        => "whois_state",

                tick => "tick_state",
            }
        ]
    );

    # and say that we want to recive said messages
    $poe_kernel->post($self->{IRCNAME}, 'register', 'all');

    # run
    $poe_kernel->run() if !$self->{no_run};
    return;
}
