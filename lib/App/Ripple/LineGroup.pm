package App::Ripple::LineGroup;

use 5.010;

use warnings;
use strict;

use base qw(Class::Accessor);
__PACKAGE__->mk_accessors(qw(properties));

use Data::Compare ();

sub add {
    my ($self, $line) = @_;

    # if this is the first object, just add it
    if ($self->count == 0) {
        $self->_add_internal($line);
        return 1;
    }

    # if its not the same kind of thing as us then we can't go any further
    if ((!$self->properties->{lineType} && !$line->properties->{lineType}) ||
        $self->properties->{lineType} ne $line->properties->{lineType}) {

        return 0;
    }

    # if there's a subgroup, try to add it to that
    if (exists $self->{subgroup}) {
        return 1 if $self->{subgroup}->add($line);

        # if the subgroup didn't like it then its finished here
        $self->_add_internal($self->{subgroup});
        delete $self->{subgroup};
    }

    # if its exactly the same as us, we can add it
    if (Data::Compare::Compare($self->properties, $line->properties)) {
        $self->_add_internal($line);
        return 1;
    }

    # if it looks like us, just add it here
    if ((!exists $self->properties->{indent} && !exists $line->properties->{indent}) ||
        ($self->properties->{indent} == $line->properties->{indent})) {
        $self->_add_internal($line);
        return 1;
    }

    # it doesn't look like us, so we either need to make a new subgroup or
    # tell the parent that we're finished, because we can't handle it
 
    my $self_indent = $self->properties->{indent} // 0;
    my $line_indent = $line->properties->{indent} // 0;

    # if we're further down the tree (greater indent level), our group
    # is finished
    if ($self_indent > $line_indent) {
        return 0;
    }

    # otherwise we're going down the tree
    $self->{subgroup} = App::Ripple::LineGroup->new;
    $self->{subgroup}->add($line);

    return 1;
}

sub _add_internal {
    my ($self, $object) = @_;

    push @{$self->{objects}}, $object;

    $self->properties($object->properties) if $self->count == 1;
}

sub count {
    my ($self) = @_;

    return scalar @{$self->{objects} || []};
}

sub start {
    my ($self) = @_;

    return -1 if !$self->count;

    return $self->{objects}->[0]->start;
}

sub end {
    my ($self) = @_;

    return -1 if !$self->count;

    return $self->{objects}->[-1]->end;
}

sub render {
    my ($self) = @_;

    if (exists $self->{subgroup}) {
        $self->_add_internal($self->{subgroup});
        delete $self->{subgroup};
    }

    return '' if !exists $self->{objects};

    my $props = $self->properties;

    my $out = '';

    my $block_close;
    my $block_elem = exists $props->{lineType} && $props->{lineType} eq "li" ? "ul" : "div";

    my (@class, @style);

    push @class, "indent" if $props->{indent};

    push @style, "direction: rtl" if exists $props->{direction} && $props->{direction} eq 'r';

    push @style, "text-align: ".($props->{alignment} eq 'c' ? "center"  :
                                 $props->{alignment} eq 'r' ? "right"   :
                                 $props->{alignment} eq 'j' ? "justify" :
                                                              "left") if exists $props->{alignment} && $props->{alignment} ne 'l';

    if ($block_elem eq "ul" || @class || @style) {
        $out .= q{<}.$block_elem;
        if (@class) {
            $out .= q{ class='}.join(' ', @class).q{'};
        }
        if (@style) {
            $out .= q{ style='}.join('; ', @style).q{;'};
        }
        $out .= q{>};

        $block_close = 1;
    }

    for my $object (@{$self->{objects}}) {
        $out .= $object->render;
    }

    $out .= q{</}.$block_elem.q{>} if $block_close;

    #$out .= sprintf q{<pre>LINEGROUP [%d %d %D]: %s</pre>}, $self->count, $self->start, $self->end, Data::Dumper::Dumper($self->properties);

    return $out;
}

1;
