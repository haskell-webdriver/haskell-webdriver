#!/usr/bin/perl
package Rolebot::Plugins::Haskell::Load;
use strict; use warnings;

use Rolebot::Bot;
use Rolebot::Config;
use File::Temp qw(tempfile);
use open qw( :encoding(UTF-8) :std);


my $parse_haskell = qr/^> (.*)$/p;
my $parse_htype   = qr/^:t (.*)$/p;
my ($temp, $temp_name) = tempfile;

sub set_temp {
    truncate $temp, 0;
    seek $temp, 0, 0;
    print $temp @_;
}

sub unload {
    close $temp;
}


my $extensions = "-XUnboxedTuples -XMagicHash -XUnicodeSyntax -XExplicitForAll -XNPlusKPatterns -XParallelListComp -XTransformListComp -XPostfixOperators -XTupleSections";

filter $parse_haskell,
sub {
    my ($self, $a, $expr) = @_;
    set_temp $expr;
    my $shell_cmd  = qq{mueval -t 7 $extensions -e "`cat $temp_name`" 2>&1};
    $shell_cmd .=' | head -n 2' if $a->{channel} ne 'msg';
    my $out = qx($shell_cmd);
    eval {
        substr($out, $Rolebot::Config::line_cap-3) = "...";
    };
    return (body => $out);
};

filter $parse_htype,
sub {
    my ($self, $a, $expr) = @_;
    set_temp ":l Plugins/Haskell/botload.hs\n:t $expr\n";
    my $shell_cmd = "ghci -v0 $extensions < $temp_name 2>&1";
    $shell_cmd   .= ' | head -n 2' if $a->{channel} ne 'msg';
    return (body => scalar qx($shell_cmd));
};
1;
