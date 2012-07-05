#!/usr/bin/perl
use Rolebot::Bot;
use Rolebot::Config;
use File::Basename;
use File::chdir;
unless (caller) {
    local $CWD = dirname $0 || $CWD;
    my $bot = Rolebot::Bot->new(
                                server      => $Rolebot::Config::server,
                                port        => $Rolebot::Config::port,
                                ssl         => $Rolebot::Config::use_ssl,
                                channels    => [@Rolebot::Config::channels],
                                nick        => $Rolebot::Config::nick,
                                alt_nicks   => [@Rolebot::Config::alt_nicks],
                                username    => $Rolebot::Config::username,
                                name        => $Rolebot::Config::name,
                               );
    $bot->run;
}

