#!/usr/bin/perl
package Rolebot::Plugins::LastSaid;
use strict; use warnings;
use v5.10;

use Rolebot::Bot;
use Rolebot::Config;

sub said {
    my ($self, $a) = @_;
    my $nick = lc $a->{who};
    my $_ = $a->{body};
    $self->state->{$nick} = $_ if $self->is_registered($nick) && !/^[\Q$Rolebot::Config::cmd_prefixes\E]lastsaid/;
    return;
}

command lastsaid => "Usage: lastsaid [<nick>] -- shows the last thing someone said while identified by services",
sub {
    my ($self, $a) = @_;
    my $nick = trim $a->{body} || $a->{who};
    my $lastsaid = $self->state->{lc $nick};
    return (body => "Nothing said by $nick") unless defined $lastsaid;
    return (body => "$nick last said: $lastsaid");
}
