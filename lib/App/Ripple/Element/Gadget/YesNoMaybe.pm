package App::Ripple::Element::Gadget::YesNoMaybe;

use 5.010;

use warnings;
use strict;

use base qw(App::Ripple::Element::Gadget);

sub render_block {
    my ($self) = @_;

    my $props = $self->properties;

    my @users = map { m/^(.*):answer$/ } keys %$props;

    my $user_sorter = sub {
        return $props->{"$a:order"} <=> $props->{"$b:order"};
    };

    my @yes   = sort $user_sorter grep { $props->{"$_:answer"} eq 'y' } @users;
    my @no    = sort $user_sorter grep { $props->{"$_:answer"} eq 'n' } @users;
    my @maybe = sort $user_sorter grep { $props->{"$_:answer"} eq 'm' } @users;

    my $render_list = sub {
        my ($name, @users) = @_;

        my $out =
            q{<div class='gadget-yesnomaybe-list gadget-yesnomaybe-}.$name.q{'>}.
                q{<div class='gadget-yesnomaybe-}.$name.q{'>}.
                    sprintf("%s (%d)", $name, scalar @users).
                q{</div>}.
                q{<ul>};

        for my $user (@users) {
            $out .=
                q{<li>}.
                App::Ripple->pretty_name($user);

            $out .= q{ - }.$props->{"$user:status"} if $props->{"$user:status"};

            $out .=
                q{</li>};
        }

        $out .=
                q{</ul>}.
            q{</div>};

        return $out;
    };

    my $out =
        q{<div class='gadget-yesnomaybe'>}.
            $render_list->("yes",   @yes).
            $render_list->("no",    @no).
            $render_list->("maybe", @maybe).
        q{</div>};

    return $out;
}

1;
