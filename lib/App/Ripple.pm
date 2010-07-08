package App::Ripple;

use 5.010;

use warnings;
use strict;

use base qw(Class::Accessor);
__PACKAGE__->mk_accessors(qw(script_uri readme_uri css_uri icon_uri));

use App::Ripple::WaveService;

use App::Ripple::Wavelet;
use App::Ripple::Blip;
use App::Ripple::Thread;
use App::Ripple::Line;
use App::Ripple::LineGroup;
use App::Ripple::Element;
use App::Ripple::Annotation;

use App::Ripple::Element::Attachment;
use App::Ripple::Element::Gadget;
use App::Ripple::Element::Image;
use App::Ripple::Element::InlineBlip;

use App::Ripple::Element::Gadget::YesNoMaybe;

sub pretty_name {
    my ($class, $name) = @_;
    $name =~ s{\@googlewave.com$}{};
    return $name;
}

1;
