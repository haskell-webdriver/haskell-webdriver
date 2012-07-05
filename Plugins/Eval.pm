#!/usr/bin/perl
use strict; use warnings;
use v5.10;
use Rolebot::Bot;
use Data::Dumper;
use open qw( :encoding(UTF-8) :std);


command perl => Admin => "Usage: perl <code>  -- evaluates perl code.",
sub {
    my ($self, $a) = @_;
    return (body => 'Nope.') unless $self->is_super_admin($a->{who});
    my $d = Data::Dumper->new([eval $a->{body}], ['result']);
    $d->Useqq(1);
    $d->Terse(1);
    $d->Indent(0);
    return (body => $d->Dump);
};
