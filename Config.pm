#!/usr/bin/perl
package Rolebot::Config;
use strict; no warnings;
use base 'Exporter';

#IRC information
our $server = 'irc.freenode.net';
our $port = 6697;
our $use_ssl = 'true';
our @channels = qw(#esoteric #esoteric-en);
our $nick = 'rolebot';
our @alt_nicks = qw(rolebot);
our $username = 'rolebot';
our $name = 'A simple RPG bot';

#WWW information
our $useragent = "Mozilla/5.0 AppleWebKit KHTML Gecko Chrome Safari";

#bot information
our @super_admins  = qw(kallisti);
our @disabled_plugins = qw();
our $cmd_prefixes = '~$';
our $line_cap  = 250;
our $max_lines = 4;
our $admins_file  = 'admins';
#our $ignore_file  = 'ignore';
our $default_help_category = '';
our $state_file_mode = 'rw';
1;
