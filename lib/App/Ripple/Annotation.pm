package App::Ripple::Annotation;

use 5.010;

use warnings;
use strict;

use base qw(Class::Accessor);
__PACKAGE__->mk_accessors(qw(blip start end name value));

use URI::Escape qw(uri_escape uri_escape_utf8);

sub markup {
    my ($self) = @_;

    my $marker = {};

    given ($self->name) {

        when (m{^style/(.*)}) {
            my $name = $1;
            $name =~ s/([A-Z])/q{-}.lc($1)/e;
            $marker->{style}->{$name} = $self->value;
        }

        when ("conv/title") {
            $marker->{style}->{'font-weight'} = 'bold';
            $marker->{style}->{'font-size'} = 'larger';
        }

        when (m{^link/(?:manual|auto)}) {
            my $href;

            if (my ($waveid) = $self->value =~ m{^waveid://(.*)}) {
                $waveid =~ s{/}{!};
                $href = main::_build_internal_uri(a => 'read', w => uri_escape($waveid));
            }
            else {
                $href = main::_build_internal_uri(a => 'redirect', u => uri_escape_utf8($self->value));
            }

            push @{$marker->{elements}}, {
                tag => 'a',
                attrs => {
                    href => $href,
                },
            };
        }

        when ("link/wave") {
            push @{$marker->{elements}}, {
                tag => 'a',
                attrs => {
                    href => main::_build_internal_uri(a => 'read', w => uri_escape($self->value)),
                },
            };
        }

    }

    return $marker;
}

1;
