package App::Ripple::OAuth;

use 5.010;

use warnings;
use strict;

use URI::Escape;
use Net::OAuth 0.25;
use LWP::UserAgent;
use Data::Random qw(rand_chars);
use Carp;

use base qw(Class::Accessor);
__PACKAGE__->mk_accessors(qw(consumer_key consumer_secret));

my $oa_scope = q{http://wave.googleusercontent.com/api/rpc};

my $oa_req_uri    = q{https://www.google.com/accounts/OAuthGetRequestToken?scope=}.uri_escape($oa_scope);
my $oa_auth_uri   = q{https://www.google.com/accounts/OAuthAuthorizeToken};
my $oa_access_uri = q{https://www.google.com/accounts/OAuthGetAccessToken};

my $wave_rpc_uri         = q{https://www-opensocial.googleusercontent.com/api/rpc};
my $wave_sandbox_rpc_uri = q{https://www-opensocial-sandbox.googleusercontent.com/api/rpc};

sub get_login_uri {
    my ($self, $callback) = @_;

    my $oa_req = Net::OAuth->request("request token")->new(
        $self->_default_request_params,
        request_url  => $oa_req_uri,
        extra_params => {
            scope => $oa_scope,
        },
    );
    $oa_req->sign;

    my $ua = LWP::UserAgent->new;
    my $res = $ua->get($oa_req->to_url);
    if (!$res->is_success) {
        croak "could not get request token: ".$res->status_line."\n".$res->content;
    }

    my $oa_res = Net::OAuth::response->("request token")->from_post_body($res->content);

    $oa_req = Net::OAuth->request("user auth")->new(
        token    => $oa_res->token,
        callback => $callback,
    );

    return ($oa_req->to_url($oa_auth_uri), $oa_res->token_secret);
}

sub handle_callback {
    my ($self, $token_secret, $callback_args) = @_;

    my $oa_res = Net::OAuth->response("user auth")->from_hash($callback_args);

    my $oa_req = Net::OAuth->request("access token")->new(
        $self->_default_request_params(),
        request_url  => $oa_access_uri,
        token        => $oa_res->token,
        token_secret => $token_secret,
    );
    $oa_req->sign;

    my $ua = LWP::UserAgent->new;
    my $res = $ua->get($oa_req->to_url);
    if (!$res->is_success) {
        croak "could not get request token: ".$res->status_line."\n".$res->content;
    }

    $oa_res = Net::OAuth->response("access_token")->from_post_body($res->content);

    return ($oa_res->token, $oa_res->token_secret);
}

sub wave_rpc_call {
}
        
sub _default_request_params {
    my ($self, $method) = @_;
    $method //= "GET";

    return (
        consumer_key     => $self->consumer_key,
        consumer_secret  => $self->consumer_secret,
        request_method   => $method,
        signature_method => "HMAC-SHA1",
        timestamp        => time,
        nonce            => join('', rand_chars(size => 16, set => "alphanumeric"))
    );
}

1;
