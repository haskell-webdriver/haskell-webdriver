#!/usr/bin/perl
package Rolebot::Plugins::Time;
use strict; use warnings;
use v5.10;
use POSIX 'strftime';
use URI::Escape 'uri_escape_utf8';
use JSON;
use DateTime;
use DateTime::TimeZone;
use Rolebot::Bot;

my $api_key = 'a5fdd7fb34162143123006';

my $date_format = '%F %R';

command time => 'Usage: time [<time_zone_or_location>] -- Displays the current time in a time zone or location. By default, shows UTC time. US zip codes are also allowed. ',
sub {
    my ($self, $args) = @_;
    my $query = trim $args->{body};
    $query = "GMT" unless length $query;
    if (DateTime::TimeZone->is_valid_name($query)) {
        my $d = DateTime->now(time_zone => $query);
        return (body =>
                "Time in "
                . $d->time_zone_short_name . ': '
                . strftime($date_format, gmtime $d->epoch));
    }
    my $json = get_url "http://www.worldweatheronline.com/feed/tz.ashx?q=$query&format=json&key=$api_key";
    my $data = decode_json($json)->{data};
    return (body => 'Invalid query.') unless defined $data && !$data->{error};
    my $city = $data->{request}->[0]->{query};
    my $time = $data->{time_zone}->[0]->{localtime};
    my $offset = $data->{time_zone}->[0]->{utcOffset};
    $offset =~ s/^(\d)/+$1/;
    $offset =~ s/\.0+$//;
    return (body => "Time in $city (GMT$offset): $time");
};
1;
