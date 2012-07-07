package Rolebot::Plugins::Echo;
use strict; use warnings;
use v5.10;
use Rolebot::Bot;

command echo => 'Usage: echo <text>  -- repeats the given text as PRIVMSG',
sub {
    my ($self, $a) = @_;
    return (body => $a->{body});
};
