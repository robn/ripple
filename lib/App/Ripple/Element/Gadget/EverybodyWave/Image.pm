package App::Ripple::Element::Gadget::EverybodyWave::Image;

use 5.010;

use warnings;
use strict;

use base qw(App::Ripple::Element::Gadget);

sub render_block {
    my ($self) = @_;

    return App::Ripple::Element::Attachment->new({
        blip       => $self->blip,
        properties => {
            url  => $self->properties->{imgUrl},
            type => 'image/jpg',
        },
    })->render_block;
}

1;
