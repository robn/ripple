package App::Ripple::Element::Image;

use 5.010;

use warnings;
use strict;

use base qw(App::Ripple::Element);

sub render_inline {
    my ($self) = @_;

    my $props = $self->properties;

    my $template_args = {
        url     => $props->{url},
        caption => $props->{caption} // '',
    };

    return $self->blip->wavelet->app->expand_template('image', $template_args);
}

1;
