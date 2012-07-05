#!/usr/bin/perl
package Rolebot::Plugins::Admin;
use strict; use warnings;
use v5.10;

use Rolebot::Bot;
use Rolebot::Config;
local $Rolebot::Config::default_help_category = 'Admin';

sub load {
    my ($self) = @_;
    my $state = $self->state;
    $state->{ignore} //= {};
    $state->{ignore_hosts} //= {};
    $self->ignore_list(keys %{$state->{ignore}});
}

command admin => 'Usage: admin (list|add|delete) [<nick1> <nick2> <nick3> ...]  -- admin manager command',
sub {
    my($self, $a) = @_;
    my $msg;
    my ($subcmd, $arg) = parse_subcommand $a->{body};
    $arg //= '';
    given ($subcmd) {
        when ('add') {
            if (!$self->is_admin($a->{who})) {
                $msg = 'Insufficient privileges';
            }
            elsif ($arg) {
                $self->admins($self->admins, split / +/, $arg);
                $msg = 'Done';
            }
        }
        when ('delete') {
            if (!$self->is_super_admin($a->{who})) {
                $msg = 'Insufficient privileges';
            }
            elsif ($arg) {
                my @arglist = split / +/, $arg;
                $self->admins(grep {my $o = $_; !grep {$_ eq $o} @arglist} $self->admins);
                $msg = 'Done';
            }
        }
        when ('list') {
            my @admins = $self->admins;
            $msg = "Admins: @admins";
        }
    }
    return (body => $msg || $a->{help});
};

command ignore => 'Usage: ignore [<nick1> <nick2> ...] -- ignores the specified nicks',
sub {
    my ($self, $a) = @_;
    my @nicks = split /\s+/, trim $a->{body};
    return (body => "Insufficient privileges.") unless $self->is_admin($a->{who});
    for my $n (@nicks) {
        return (body => "Can't ignore admins.") if grep {lc $_ eq lc $n} $self->admins;
    }
    my $ignore = $self->state->{ignore};
    my $ignore_hosts = $self->state->{ignore_hosts};
    unless (@nicks) {
        my $sep = "\x{3}2|\x{F}";
        my $msg = $a->{help};
        $msg .= " $sep Ignored nicks: @{[keys %$ignore]}";
        #$msg .= " $sep Ignored hosts: @{[keys %$ignore_hosts]}";
        return (body => $msg );
    }
    for(@nicks) {
        $ignore->{$_} = 1;
    #    my $host = $self->pocoirc->nick_info($_)->{Host};
    #    $ignore_hosts->{$host} = 1 if $host;
    }
    $self->ignore_list(keys %$ignore);
    return (body => "Done.");
};

command unignore => 'Usage: unignore <nick1> <nick2> ... -- unignores the specified nicks',
sub {
    my ($self, $a) = @_;
    my @nicks = split /\s+/, trim $a->{body};
    return (body => $a->{help}) unless @nicks;
    return (body => "Insufficient privileges.") unless $self->is_admin($a->{who});
    my $ignore = $self->state->{ignore};
    delete $ignore->{$_} for @nicks;
    $self->ignore_list(keys %$ignore);
    return (body => "Done.");
};

command shutdown => 'Bot shutdown.',
sub {
    my ($self, $who) = (shift, shift->{who});
    $self->shutdown("manual shutdown by $who") if $self->is_super_admin($who);
    return (body=>"I'm sorry $who, I'm afraid I can't do that.");
};
