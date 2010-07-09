package App::Ripple::Blip;

use 5.010;

use warnings;
use strict;

use base qw(Class::Accessor);
__PACKAGE__->mk_accessors(qw(wavelet data blip_id));

use App::Ripple::Line;
use App::Ripple::Element;
use App::Ripple::Annotation;
use App::Ripple::LineGroup;
use App::Ripple::Thread;

use Date::Format;
use HTML::Entities;

sub new {
    my ($class, $args) = @_;

    my $self = $class->SUPER::new($args);

    $self->blip_id($self->data->{blipId});

    return $self;
}

sub render {
    my ($self) = @_;

    my $data = $self->data;

    my $out =
        q{<div class='blip' id='}.$self->blip_id.q{'>}.
            q{<b>}.$self->wavelet->app->pretty_name($data->{creator}).q{</b>}.
            time2str(q{ at <b>%l:%M%P</b> on <b>%e %B</b>}, $data->{lastModifiedTime}/1000);

    my @contributors = map { $self->app->pretty_name($_) } grep { $_ ne $data->{creator} } @{$data->{contributors}};
    if (@contributors) {
        $out .=
            q{<br />}.
            q{with <b>}.
            join (q{</b>, <b>}, @contributors).
            q{</b>};
    }

    if ($self->wavelet->debug) {
        $out .= 
            q{<div class='blip-debug'>}.
                q{blip: }.$self->blip_id.q{<br />}.
                q{parent: }.$data->{parentBlipId}.q{<br />}.
                q{thread: }.$data->{threadId}.q{<br />}.
            q{</div>};
    }

    my @line_positions = sort { $a <=> $b } grep { $data->{elements}->{$_}->{type} eq "LINE" } keys %{$data->{elements}};
    for my $i (0 .. $#line_positions) {
        my $position = $line_positions[$i];
        my $line = $data->{elements}->{$position};

        push @{$self->{lines}}, App::Ripple::Line->new({
            blip       => $self,
            start      => $position,
            end        => $i == $#line_positions ? length $data->{content} : $line_positions[$i+1],
            properties => $line->{properties},
        });
    }

    for my $position (sort { $a <=> $b } grep { $data->{elements}->{$_}->{type} ne "LINE" } keys %{$data->{elements}}) {
        my $element = $data->{elements}->{$position};

        push @{$self->{elements}}, App::Ripple::Element->new({
            blip       => $self,
            position   => $position,
            type       => $element->{type},
            properties => $element->{properties},
        });
    }

    for my $annotation (@{$data->{annotations}}) {
        push @{$self->{annotations}}, App::Ripple::Annotation->new({
            blip  => $self,
            start => $annotation->{range}->{start},
            end   => $annotation->{range}->{end},
            name  => $annotation->{name},
            value => $annotation->{value},
        });
    }

    $out .= q{<div class='blip-content'>};

    my $linegroup = App::Ripple::LineGroup->new;

    for my $line (@{$self->{lines}}) {
        if (! $linegroup->add($line)) {
            $out .= $linegroup->render;
            $out .= $_->render_block for $self->elements_in_range($linegroup->start, $linegroup->end);

            $linegroup = App::Ripple::LineGroup->new;
            $linegroup->add($line);
        }
    }

    $out .= $linegroup->render;
    $out .= $_->render_block for $self->elements_in_range($linegroup->start, $linegroup->end);

    $out .= q{</div>}.q{</div>};

    if (@{$data->{replyThreadIds}}) {
        for my $thread_blip_id (@{$data->{replyThreadIds}}) {
            next if $self->wavelet->data->{threads}->{$thread_blip_id}->{location} != -1;

            $out .= App::Ripple::Thread->new({
                wavelet  => $self->wavelet,
                blip_ids => $self->wavelet->data->{threads}->{$thread_blip_id}->{blipIds},
            })->render;
        }
    }

    return $out;
}

sub content_range {
    my ($self, $start, $end) = @_;

    return encode_entities(substr $self->data->{content}, $start, $end-$start);
}

sub annotated_content_range {
    my ($self, $start, $end) = @_;

    # what we want to do here is find any annotations that are either
    # completely or partially inside the range. then we convert them into
    # linear set of elements (eg anchors or styled spans) such that none of
    # them overlap.
    #
    # that is, given the following content range:
    #
    #            |========== content range ==========|
    #
    # and a set of annotations over that range:
    #
    #       |----- A -----|
    #                 |------ B ------|
    #          |----------- C -----------|
    #                                        |-- D --|
    #
    # produce the following set of spans:
    #
    #            | AC |ABC|---- BC ---|C |   |-- D --|

    # loop over the annotations and figure out where all the boundaries are.
    # while we're at it, build a list of annotations with coerced ranges such
    # that they always fall inside the wanted range
    my %boundaries;
    my @coerced_annotations;
    for my $annotation (@{$self->{annotations}}) {
        next if ! (
            ($annotation->start >= $start && $annotation->start <  $end) ||
            ($annotation->end   >  $start && $annotation->end   <= $end) ||
            ($annotation->start <  $start && $annotation->end   >  $end)
        );

        my $boundary_start = $annotation->start < $start ? $start : $annotation->start;
        my $boundary_end   = $annotation->end   > $end   ? $end   : $annotation->end;

        push @coerced_annotations, App::Ripple::Annotation->new({
            start => $boundary_start,
            end   => $boundary_end,
            name  => $annotation->name,
            value => $annotation->value,
        });

        $boundaries{$boundary_start} = [];
        $boundaries{$boundary_end}   = [];
    }

    # make boundaries for elements as well. this allows us to get eg inline
    # images for free
    my %element_positions;
    for my $element (@{$self->{elements}}) {
        my $position = $element->position;
        next if !($position >= $start && $position <= $end);

        $boundaries{$position} = [] if not exists $boundaries{$position};
        push @{$element_positions{$position}}, $element;
    }

    my @positions = sort { $a <=> $b } keys %boundaries;
    if (!@positions) {
        # no positions recorded so there's nothing here and the raw content
        # range will be fine
        return $self->content_range($start, $end);
    }

    # loop over the boundary positions and attach a list of split annotations
    # that start at that position to each
    for my $i (0 .. $#positions-1) {
        my $position = $positions[$i];

        my @start_annotations = grep { $_->start <= $position && $_->end > $position } @coerced_annotations;

        for my $annotation (@start_annotations) {
            my $split_annotation = App::Ripple::Annotation->new({
                start => $position,
                end   => $positions[$i+1],
                name  => $annotation->name,
                value => $annotation->value,
            });

            push @{$boundaries{$position}}, $split_annotation;
            push @{$boundaries{$positions[$i+1]}}, $split_annotation;
        }
    }

    # loop over the boundary positions and produce appropriate output for each
    my $content = '';
    for my $i (0 .. $#positions) {
        my $position = $positions[$i];

        my @start_annotations = grep { $_->start == $position } @{$boundaries{$position}};
        my @end_annotations   = grep { $_->end   == $position } @{$boundaries{$position}};

        if (@end_annotations) {
            my (@elems, $style);

            for my $annotation (@end_annotations) {
                my $marker = $annotation->markup;

                push @elems, $_ for @{$marker->{elements}};
                $style += keys %{$marker->{style}};
            }

            $content .= q{</}.$_.q{>} for map { $_->{tag} } reverse @elems;
            $content .= q{</span>} if $style;
        }

        for my $element (@{$element_positions{$position}}) {
            $content .= $element->render_inline;
        }

        if (@start_annotations) {
            my (@elems, %style);

            for my $annotation (@start_annotations) {
                my $marker = $annotation->markup;

                push @elems, $_ for @{$marker->{elements}};
                $style{$_} = $marker->{style}->{$_} for keys %{$marker->{style}};
            }

            $content .= q{<span style='}.join('; ', map { "$_: $style{$_}" } keys %style).q{'>} if keys %style;
            for my $elem (@elems) {
                $content .=
                    q{<}.$elem->{tag}.
                    join(q{}, map { " $_='".encode_entities($elem->{attrs}->{$_})."'" } keys %{$elem->{attrs}}).
                    q{>};
            }
        }

        if ($i < $#positions) {
            $content .= $self->content_range($position, $positions[$i+1]);
        }

    }

    return $content;
}

sub elements_in_range {
    my ($self, $start, $end) = @_;

    return grep { $_->position >= $start && $_->position <= $end } @{$self->{elements}};
}

1;
