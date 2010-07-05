package App::Ripple::Element;

use 5.010;

use warnings;
use strict;

use base qw(Class::Accessor);
__PACKAGE__->mk_accessors(qw(blip position type properties));

# XXX replace with a plugin registration thing
my %element_classes = (
    ATTACHMENT  => "App::Ripple::Element::Attachment",
    IMAGE       => "App::Ripple::Element::Image",
    INLINE_BLIP => "App::Ripple::Element::InlineBlip",
    GADGET      => "App::Ripple::Element::Gadget",
);

sub new {
    my ($class, $args) = @_;

    if ($class ne __PACKAGE__) {
        return $class->SUPER::new($args);
    }

    if (my $target_class = $element_classes{$args->{type}}) {
        return $target_class->new($args);
    }

    return $class->SUPER::new($args);
}

sub render_block {
    return '';
}

sub render_inline {
    return '';
}

1;
