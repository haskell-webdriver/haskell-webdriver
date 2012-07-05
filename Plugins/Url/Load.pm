#!/usr/bin/perl
package Rolebot::Plugins::Url::Load;
use strict; use warnings;
use v5.10;

use Rolebot::Bot;
use HTML::Entities ();

my $parse_url = qr{(?:(?:(?:https?|ftp)://)|www\.)(?:(?:[0-9]{1,3}\.){3}[0-9]{1,3}|(?:[a-z0-9\-]+\.)*[a-z0-9\-]+\.[a-z]{2,4})(?::[0-9]+)?(?:[/?]\S*[^\s,;\.:">])?}mips;



my %stop_words;

sub load {
    open my $f, 'Plugins/Url/stop_words.txt';
    while(<$f>) {
        chomp;
        $stop_words{lc$_}++;
    }
}

filter $parse_url,
sub {
    my ($self, $a) = @_;
    return unless $a->{channel} ne 'msg';
    my @titles = ();
    while (@titles < 5 && $a->{body} =~ /$parse_url/gp) {
        my $link = ${^MATCH};
        $link = "http://" . $link if $link =~ /^www/;
        if (get_url($link) =~ m{<title>(.*?)</title>}si) {
            my $title = HTML::Entities::decode($1);
            $title =~ s/\s+/ /g;
            $title = trim (substr $title, 0, 200);
            my $is_redundant = 1;
            for (split /[^\w']+/, $title) {
                unless ($stop_words{lc$_} || $link =~ /\Q$_\E/i) {
                    $is_redundant = 0;
                    last;
                }
            }
            unless ($is_redundant) {
                $title .= "..." if length $title >= 200;
                push @titles, $title;
            }
        }
    }
    return (body => join(" \x{3}2|\x{F} ", @titles));
};
1;
