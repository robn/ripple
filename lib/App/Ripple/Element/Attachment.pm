package App::Ripple::Element::Attachment;

use 5.010;

use warnings;
use strict;

use base qw(App::Ripple::Element);

sub render_block {
    my ($self) = @_;

    my $props = $self->properties;

    my $caption = $props->{caption} ? $props->{caption} : $props->{attachmentId};
    my $icon = $main::icon_type_map{$props->{mimeType}} ? $main::icon_type_map{$props->{mimeType}} : $main::icon_type_map{_unknown};

    my $type = !exists $props->{mimeType} || $props->{mimeType} =~ m{^image/(?:png|gif|jpeg)$} ? "image" : "attachment";

    my $url = $props->{attachmentUrl} || $props->{url};

    my $out =
        q{<div class='}.$type.q{' id='}.$props->{attachmentId}.q{'>}.
            q{<a href='}.$url.q{'}.
                q{<img}.
                    q{ src='}.($type eq "image" ? $url : $main::r->icon_uri."/".$icon).q{'}.
                    q{ alt='}.$caption.q{'}.
                q{ />}.
                q{<br />}.
                $caption.
            q{</a>}.
        q{</div>};

    return $out;
}

1;
