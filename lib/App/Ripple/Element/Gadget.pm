package App::Ripple::Element::Gadget;

use 5.010;

use warnings;
use strict;

use base qw(App::Ripple::Element);

use HTML::Entities;
use Data::Dumper;

# XXX replace with a plugin registration thing
my %gadget_classes = (
    "http://wave-api.appspot.com/public/gadgets/areyouin/gadget.xml" => "App::Ripple::Element::Gadget::YesNoMaybe",
    "http://everybodywave.appspot.com/gadget/image/gadget.xml"       => "App::Ripple::Element::Gadget::EverybodyWave::Image",
);

sub new {
    my ($class, $args) = @_;

    if ($class ne __PACKAGE__) {
        return $class->SUPER::new($args);
    }

    if (my $target_class = $gadget_classes{$args->{properties}->{url}}) {
        return $target_class->new($args);
    }

    return $class->SUPER::new($args);
}

sub render_block {
    my ($self) = @_;

    my $props = $self->properties;

    my $template_args = {};

    $template_args->{url} = encode_entities($props->{url});

    if ($self->blip->wavelet->debug) {
        $template_args->{debug} = encode_entities(Dumper($props));
    }

    return $self->blip->wavelet->app->expand_template('gadget', $template_args);
}

1;
