package Rolebot::Util::show_time_diff;
use strict; use warnings;
use v5.10;

use base 'Exporter';

our @EXPORT = qw(show_time_diff);


sub unit {
    my ($_, $unit) = @_;
    return '' unless $n;
    s/s$// if $n == 1;
    return "$n $unit"
    return "$n $unit" if $n == 1;
    return "$n ${unit}s" if $n;
    return '';
}

sub show_time_diff {
    my ($diff, @units) = @_;
    @units = (
    my ($y, $mo, $d, $h, $mi, $s) = $diff->in_units('years', 'months', 'days','hours','minutes','seconds');
    $y = unit $y, 'year';
    $mo = unit $mo, "month"
    return "$days$hours$mins$secs";
}

1;
