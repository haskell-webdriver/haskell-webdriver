#!/usr/bin/perl
package Rolebot::Plugins::Words::Load;
use strict; use warnings;
use Rolebot::Bot;
use IPC::Open3;
use v5.10;
use open qw( :encoding(UTF-8) :std);

my $words_path = "$Rolebot::Bot::bot_dir/Plugins/Words/words/words.pl";

command words => "Use words --help for help",
sub {
    my ($self, $a) = @_;
    my ($out, $err);
    my $pid = open3(undef, $out, $err, $words_path, split / +/, $a->{body});
    waitpid($pid, 0);
    binmode $out, ":encoding(UTF-8)";
    if(defined $err) {
        binmode $err, ":encoding(UTF-8)";
        print <$err> if $err;
    }
    return (body => join '', <$out>);
}
