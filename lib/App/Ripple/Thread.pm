package App::Ripple::Thread;

use 5.010;

use warnings;
use strict;

use base qw(Class::Accessor);
__PACKAGE__->mk_accessors(qw(wavelet blip_ids));

sub render {
    my ($self) = @_;

    my @blips = map { $self->wavelet->blip($_) // () } @{$self->blip_ids};
    return if !@blips;

    my $thread_html = '';
    $thread_html .= $_->render for @blips;
 
    return $self->wavelet->app->expand_template('thread', {
        thread_html => $thread_html,
        wave_id     => $self->wavelet->wave_id,
        wavelet_id  => $self->wavelet->wavelet_id,
        blip_id     => $self->blip_ids->[-1],
    });
}

1;
