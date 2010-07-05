package App::Ripple::Line;

use 5.010;

use warnings;
use strict;

use base qw(Class::Accessor);
__PACKAGE__->mk_accessors(qw(blip start end properties));

sub render {
    my ($self) = @_;

    my $out = '';

    my $content = $self->blip->annotated_content_range($self->start, $self->end);
    my $properties = $self->properties;

    if (!exists $properties->{lineType}) {
        $out .= $content.q{<br />};
    }
    else {
        $out .=
            q{<}.$properties->{lineType}.q{>}.
            $content.
            q{</}.$properties->{lineType}.q{>};
    }

    #$out .= sprintf q{<pre>LINE [%d %d]: %s</pre>}, $self->start, $self->end, Data::Dumper::Dumper($self->properties);

    return $out;
}

1;
