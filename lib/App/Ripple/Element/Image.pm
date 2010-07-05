package App::Ripple::Element::Image;

use 5.010;

use warnings;
use strict;

use base qw(App::Ripple::Element);

sub render_inline {
    my ($self) = @_;

    my $props = $self->properties;

    my $out = q{<img src='}.$props->{url}.q{' };
    $out .= q{alt='}.$props->{caption}.q{' } if exists $props->{caption};
    $out .= q{/>};

    return $out;
}

1;
