package App::Ripple::Element::InlineBlip;

use 5.010;

use warnings;
use strict;

use base qw(App::Ripple::Element);

use App::Ripple::Thread;

sub render_block {
    my ($self) = @_;

    return '' if ! $self->blip->wavelet->data->{threads}->{$self->properties->{id}}->{blipIds};

    return App::Ripple::Thread->new({
        wavelet  => $self->blip->wavelet,
        blip_ids => $self->blip->wavelet->data->{threads}->{$self->properties->{id}}->{blipIds},
    })->render;
}

sub render_inline {
    my ($self) = @_;

    return '' if ! $self->blip->wavelet->data->{threads}->{$self->properties->{id}}->{blipIds};

    my $template_args = {
        blip_id => $self->properties->{id},
    };

    return $self->blip->wavelet->app->expand_template('inline_blip', $template_args);
}

1;
