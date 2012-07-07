#!/usr/bin/perl
package Rolebot::Plugins::Msg;
use strict; use warnings;
use Rolebot::Bot;
use Rolebot::Config;
use DateTime;
use DateTime::Format::Human::Duration;
local $Rolebot::Config::default_help_category = "Messages";

my %has_told = ();

sub show_messages {
    my ($self, $nick) = @_;
    $nick = lc $nick;
    my $state = $self->state;
    return unless my $count = @{$state->{$nick} // []} and $self->is_registered($nick);
    my ($s,$p) = $count == 1? ('','it') : ('s','them');
    my $cmd    = substr ($Rolebot::Config::cmd_prefixes, 0,1) . 'messages';
    $has_told{$nick} = 1;
    return (address => 1, body => "You have $count message$s. Type $cmd to read $p.");
}

sub chanjoin {
    my ($self, $a) = @_;
    show_messages $self, $a->{who};
}

sub userquit {
    my ($self, $a) = @_;
    $has_told{lc $a->{who}} = 0;
    return;
}

sub chanpart {
    my ($self, $a) = @_;
    $has_told{lc $a->{who}} = 0;
    return;
}

sub got_names {
    my ($self, $a) = @_;
    #show_messages $self, $_ for $a->{names};
}

sub said {
    my ($self, $a) = @_;
    my $who = $a->{who};
    return show_messages $self, $who unless $has_told{lc $who};
    return;
}

command tell => q(Usage: tell <name> <message> -- Leave someone a message that they'll get when they're online. Messages are sent securely and privately. Both parties must be registered with services.),
sub {
    my ($self, $a) = @_;
    my ($target, $msg) = map {lc} ($a->{body} =~ /^ *(\S+) +(.+?) *$/);
    return (body => $a->{help}) unless $target && $msg;
    my $who = lc $a->{who};
    return (body => "Tell yourself.") if $target eq $who;
    return (body => "Cool!") if $target eq lc $self->nick;
    return (body => "You must be registered to send messages.") unless $self->is_registered($who);
    push @{$self->state->{$target}}, [$who,$2,time];
    $has_told{$target} = 0;
    return (body => 'Done.');
};


command messages => q(Displays all of your unread messages and then deletes them. Messages are received securely and privately. Both parties must be registered with services.),
sub {
    my ($self, $a) = @_;
    my $who = lc $a->{who};
    return (body=>'You must be registered with services to view messages.') unless $self->is_registered($who);
    my $state = $self->state;
    return (body=>'You have no messages.') unless my $msgs = $state->{$who};
    my $out;
    for(@$msgs) {
        my ($sender, $msg, $time) = @$_;
        my $span = DateTime::Format::Human::Duration->new;
        my $odate = DateTime->from_epoch(epoch => $time);
        my $now = DateTime->now;
        my $tdiff = $span->format_duration_between($now, $odate);
        $out .= "Sent by $sender $tdiff ago: $msg\n";
    }
    delete $state->{$who};
    $has_told{$who} = 1;
    $self->say(%$a, address => 1, body => "See PM.") if $a->{channel} ne 'msg';
    return (channel => 'msg', body => $out);
};
1;
