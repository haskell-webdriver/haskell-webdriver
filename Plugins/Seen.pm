#!/usr/bin/perl
package Rolebot::Plugins::Seen;
use strict; use warnings;

use Rolebot::Bot;
use DateTime;
use DateTime::Format::Human::Duration;
use POSIX 'strftime';

our $seen_date_format = '%A %B %d, %Y at %R GMT';

sub seen {
    my ($self, @nicks) = @_;
    for(@nicks) {
        next unless $self->is_registered($_);
        $self->state->{lc $_} = time;
    }
    return;
}

sub chanjoin {
    my ($self, $a) = @_;
    seen $self, $a->{who};
}

sub chanpart {
    my ($self, $a) = @_;
    seen $self, $a->{who};
}

sub nick_change {
    my ($self, $old, $new) = @_;
    seen $self, $new;
}

sub got_names {
    my ($self, $a) = @_;
    #seen $self, keys %{$a->{names}};
}

sub said {
    my ($self, $a) = @_;
    seen $self, $a->{who};
}

command seen => 'Usage: seen <nickname> -- Shows the last time someone was seen online and identified by services.',
sub {
    my ($self, $a) = @_;
    my $nick = trim $a->{body};
    return (body => $a->{help}) unless $nick;
    my $t = $self->state->{lc $nick};
    return (body => "No one named $nick has been seen.") unless $t;
    my $span = DateTime::Format::Human::Duration->new;
    my $odate = DateTime->from_epoch(epoch => $t);
    my $now = DateTime->now;
    my $difftime = $span->format_duration_between($now, $odate,
                                                 past => '%s ago',
                                                 no_time => 'just now');
    my $strtime = POSIX::strftime ($seen_date_format, gmtime $t);
    return (body => "$nick was last seen on $strtime ($difftime)");
};
1;
