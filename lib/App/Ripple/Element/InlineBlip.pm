package App::Ripple::Element::InlineBlip;

use 5.010;

use warnings;
use strict;

use base qw(App::Ripple::Element);

use App::Ripple::Thread;

sub render_block {
    my ($self) = @_;

    return App::Ripple::Thread->new({
        wavelet  => $self->blip->wavelet,
        blip_ids => $self->blip->wavelet->data->{threads}->{$self->properties->{id}}->{blipIds},
    })->render;
}

sub render_inline {
    my ($self) = @_;

    return
        q{ <a href='#}.$self->properties->{id}.q{'>}.
            q{<img src='}.$self->blip->wavelet->app->icon_uri.q{/inline-blip.png' alt='[link to inline discussion]' />}.
        q{</a> };
}

1;
