package App::Ripple;

use 5.010;

use warnings;
use strict;

use base qw(Class::Accessor);
__PACKAGE__->mk_accessors(qw(script_uri readme_uri css_uri icon_uri template_path consumer_key consumer_secret debug));

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

use Template;

sub waveservice {
    my ($self) = @_;

    return $self->{waveservice} //= App::Ripple::WaveService->new({
        consumer_key    => $self->consumer_key,
        consumer_secret => $self->consumer_secret,
    });
}
    
sub pretty_name {
    my ($self, $name) = @_;
    $name =~ s{\@googlewave.com$}{};
    return $name;
}

sub build_internal_uri {
    my ($self, %args) = @_;

    $args{d} = 1 if $self->debug;

    my $fragment = delete $args{'#'};

    return $self->script_uri . (keys %args ? q{?}.join(q{&}, map { "$_=$args{$_}" } keys %args) : q{}) . ($fragment ? '#'.$fragment : q{});
}

sub expand_template {
    my ($self, $template, $vars) = @_;

    $self->{template_engine} //= Template->new({
        INCLUDE_PATH => [$self->template_path],
    });

    $vars //= {};

    $vars->{script_uri} //= $self->script_uri;
    $vars->{readme_uri} //= $self->readme_uri;
    $vars->{css_uri}    //= $self->css_uri;
    $vars->{icon_uri}   //= $self->icon_uri;

    $self->{template_engine}->process("$template.html", $vars, \my $out);

    return $out;
}

1;
