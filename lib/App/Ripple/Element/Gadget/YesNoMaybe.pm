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

    my $template_args = {};

    my $render_list = sub {
        my ($name, @users) = @_;

        $template_args->{$name}->{count} = @users;

        for my $user (@users) {
            my $user_args = {
                name => $self->blip->wavelet->app->pretty_name($user),
            };
            $user_args->{status} = $props->{"$user:status"} if $props->{"$user:status"};

            push @{$template_args->{$name}->{users}}, $user_args;
        }
    };

    $render_list->("yes",   @yes).
    $render_list->("no",    @no).
    $render_list->("maybe", @maybe).

    return $self->blip->wavelet->app->expand_template('gadget_yesnomaybe', $template_args);
}

1;
