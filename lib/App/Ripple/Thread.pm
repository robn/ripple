package App::Ripple::Thread;

use 5.010;

use warnings;
use strict;

use base qw(Class::Accessor);
__PACKAGE__->mk_accessors(qw(wavelet blip_ids));

sub render {
    my ($self) = @_;

    return if !@{$self->blip_ids};

    my $out = q{<div class='thread'>};

    for my $blip_id (@{$self->blip_ids}) {
        $out .= $self->wavelet->blip($blip_id)->render;
    }

    $out .= main::_reply_textarea($self->wavelet->wave_id, $self->wavelet->wavelet_id, $self->blip_ids->[-1]);

    $out .= q{</div>};

    return $out;
}

1;
