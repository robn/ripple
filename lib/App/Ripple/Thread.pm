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

    $out .=
        q{<div class='blip-reply'>}.
            main::_form_wrap( { method => 'post' },
               [qw(hidden w),  $self->wavelet->wave_id    ],
               [qw(hidden wl), $self->wavelet->wavelet_id ],
               [qw(hidden b),  $self->blip_ids->[-1]      ],
               [qw(textarea r)],
               [qw(submit a reply)], 
            ).
        q{</div>};

    $out .= q{</div>};

    return $out;
}

1;
