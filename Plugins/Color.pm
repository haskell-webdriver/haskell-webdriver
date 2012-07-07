package Rolebot::Plugins::Color;
use strict; use warnings;
use v5.10;
use Rolebot::Bot;


sub can_use_color {
    my ($self, $channel) = @_;
    return $channel eq 'msg' || !$self->pocoirc->is_channel_mode_set($channel, 'c') || $self->pocoirc->nick_channel_modes($channel, $self->nick) =~ /h|o|v|q/;
}

sub msg_filter {
    my ($self, $a) = @_;
    $a->{body} =~ s/\x{3}\d{1,2}|[\x{F}\x{2}\26\37]//g unless can_use_color($self, $a->{channel});
}
