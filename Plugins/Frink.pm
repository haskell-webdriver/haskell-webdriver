#!/usr/bin/perl
package Rolebot::Plugins::Frink;
use strict; use warnings;
use v5.10;

use Rolebot::Bot;

my $max_memory = 200000;  #in kilobytes
my $max_time   = 8;       #in seconds

command frink => "Usage: frink <expression> -- Executes a frink expression. Frink is a powerful calculator program. See http://futureboy.us/frinkdocs/ for more information.",
sub {
    my ($self, $a) = @_;
    return (body => $a->{help}) unless my $expr = $a->{body};
    my ($charlimit, $linelimit) = $a->{channel} eq 'msg'? (2000,20) : (200,2);
    my $out = qx(
        ulimit -t $max_time;
        ulimit -m $max_memory;
        frink --sandbox -e '$expr'| head -c $charlimit | head -n $linelimit
    );
    return (body => $out);
}
