package App::Ripple::Element::Attachment;

use 5.010;

use warnings;
use strict;

use base qw(App::Ripple::Element);

use URI::Escape qw(uri_escape_utf8);

my %icon_type_map = (
    '_unknown'   => 'unknown.png',
);

sub render_block {
    my ($self) = @_;

    my $props = $self->properties;

    my $type = !exists $props->{mimeType} || $props->{mimeType} =~ m{^image/(?:png|gif|jpeg)$} ? "image" : "attachment",

    my $icon = $icon_type_map{$props->{mimeType}} ? $icon_type_map{$props->{mimeType}} : $icon_type_map{_unknown};

    my $url = $self->blip->wavelet->app->build_internal_uri(a => 'redirect', u => uri_escape_utf8($props->{attachmentUrl} || $props->{url}));

    my $template_args = {
        attachment_id      => $props->{attachmentId},
        attachment_url     => $url,
        attachment_image   => $type eq "image" ? $url : $self->blip->wavelet->app->icon_uri."/".$icon,
        attachment_caption => $props->{caption} ? $props->{caption} : $props->{attachmentId},
    };

    return $self->blip->wavelet->app->expand_template('attachment', $template_args);
}

1;
