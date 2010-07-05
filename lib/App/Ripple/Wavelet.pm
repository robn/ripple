package App::Ripple::Wavelet;

use 5.010;

use warnings;
use strict;

use base qw(Class::Accessor);
__PACKAGE__->mk_accessors(qw(data debug wave_id wavelet_id title));

use App::Ripple::Thread;
use App::Ripple::Blip;

use HTML::Entities;
use Data::Dumper;

sub new {
    my ($class, $args) = @_;

    my $self = $class->SUPER::new($args);

    $self->wave_id($self->data->{waveletData}->{waveId});
    $self->wavelet_id($self->data->{waveletData}->{waveletId});

    return $self;
}

sub title {
    my ($self) = @_;

    if (my $title = $self->_title_accessor) {
        return $title;
    }

    my $blip = $self->blip($self->data->{waveletData}->{rootBlipId});
    my ($title_annotation) = grep { $_->{name} eq "conv/title" } @{$blip->data->{annotations}};

    return $self->_title_accessor($blip->content_range($title_annotation->{range}->{start}, $title_annotation->{range}->{end}));
}

sub render {
    my ($self) = @_;

    my $data = $self->data;

    my $out =
        q{<div class='wavelet'>}.
            q{<div class='wavelet-action-box'>}.
                main::_form_wrap(
                    [qw(hidden w), $self->wave_id],
                    [qw(hidden wl), $self->wavelet_id],
                    [qw(hidden a add)],
                    [qw(submit), undef, q{add people}],
                ).
            q{</div>}.
            q{In this wave: }.
            q{<b>}.join(q{</b>, <b>}, main::_pretty_names(@{$data->{waveletData}->{participants}})).q{</b>}.
        q{</div>};

    $out .= App::Ripple::Thread->new({
        wavelet  => $self, 
        blip_ids => $data->{waveletData}->{rootThread}->{blipIds}
    })->render;

    if ($self->debug) {
        $out .=
            q{<div class='protocol-debug'>}.
                q{<pre>}.
                    encode_entities(Dumper($data)).
                q{</pre>}.
            q{</div>};
    }

    return $out;
}

sub blip {
    my ($self, $blip_id) = @_;

    my $blipdata = $self->data->{blips}->{$blip_id};
    return if not $blipdata;

    return App::Ripple::Blip->new({ wavelet => $self, data => $blipdata });
}

1;
