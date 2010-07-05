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

    my $out =
        q{<div class='gadget-unknown'>}.
            q{GADGET: }.encode_entities($props->{url}).
        q{</div>};

    if ($self->blip->wavelet->debug) {
        $out .=
            q{<div class='protocol-debug'>}.
                q{<pre>}.
                    encode_entities(Dumper($props)).
                q{</pre>}.
            q{</div>};
    }

    return $out;
}

1;
