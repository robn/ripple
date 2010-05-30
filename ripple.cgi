#!/usr/bin/env perl

use 5.010;

use warnings;
use strict;

use URI::Escape;
use Crypt::OpenSSL::RSA;
use CGI::Carp qw(fatalsToBrowser);
use Net::OAuth;
use Data::Random qw(rand_chars);
use LWP::UserAgent;
use CGI ();
use JSON qw(decode_json encode_json);
use Data::Dumper;

my $consumer_key     = "anonymous";
my $consumer_secret  = "anonymous";

my $scope = q{http://wave.googleusercontent.com/api/rpc};

my $oa_req_uri    = q{https://www.google.com/accounts/OAuthGetRequestToken?scope=}.uri_escape($scope);
my $oa_auth_uri   = q{https://www.google.com/accounts/OAuthAuthorizeToken};
my $oa_access_uri = q{https://www.google.com/accounts/OAuthGetAccessToken};

my $rpc_uri = q{https://www-opensocial.googleusercontent.com/api/rpc};


my $base_uri = "http://junai/ripple/ripple.cgi";

my $q = CGI->new;

if ($q->param("s") eq "callback") {
    do_callback();
}
elsif ($q->param("s") eq "logout") {
    do_logout();
}
elsif ($q->cookie("token") && $q->cookie("secret")) {
    do_wave();
}
else {
    do_login();
}

exit 0;

sub do_login {
    my $oa_req = Net::OAuth->request("request token")->new(
        _default_request_params(),
        request_url => $oa_req_uri,
        extra_params => {
            scope => $scope,
        },
    );
    $oa_req->sign;

    my $ua = LWP::UserAgent->new;
    my $res = $ua->get($oa_req->to_url);

    if (!$res->is_success) {
        die "could not get request token: ".$res->status_line."\n".$res->content;
    }

    my $oa_res = Net::OAuth->response("request token")->from_post_body($res->content);

    $oa_req = Net::OAuth->request("user auth")->new(
        token        => $oa_res->token,
        callback     => "$base_uri?s=callback",
    );

    my $secret_cookie = $q->cookie(-name => "secret", -value => $oa_res->token_secret);

    print $q->redirect(-uri => $oa_req->to_url($oa_auth_uri), -cookie => [$secret_cookie]);
}

sub do_callback {
    my $oa_res = Net::OAuth->response("user auth")->from_hash({$q->Vars});

    my $oa_req = Net::OAuth->request("access token")->new(
        _default_request_params(),
        request_url  => $oa_access_uri,
        token        => $oa_res->token,
        token_secret => $q->cookie("secret"),
    );
    $oa_req->sign;

    my $ua = LWP::UserAgent->new;
    my $res = $ua->get($oa_req->to_url);

    if (!$res->is_success) {
        die "could not get access token: ".$res->status_line."\n".$res->content;
    }

    $oa_res = Net::OAuth->response("access token")->from_post_body($res->content);

    my $token_cookie = $q->cookie(-name => "token", -value => $oa_res->token);
    my $secret_cookie = $q->cookie(-name => "secret", -value => $oa_res->token_secret);

    print $q->redirect(-uri => $base_uri, -cookie => [$token_cookie, $secret_cookie]);
}

sub do_logout {
    my $token_cookie = $q->cookie(-name => "token", -value => "");
    my $secret_cookie = $q->cookie(-name => "secret", -value => "");

    print $q->redirect(-uri => $base_uri, -cookie => [$token_cookie, $secret_cookie]);
}

sub do_wave {
    my %action_handler = (
        inbox  => \&action_inbox,
        search => \&action_search,
        test   => \&action_test,
    );

    my $out;

    my $action = $q->param("a");
    if ($action && exists $action_handler{$action}) {
        $out = $action_handler{$action}->();
    }

    _html_header();

    _form_wrap(
        [qw(submit a inbox)],
        [qw(submit a test)],
        [qw(submit s logout)],
    );

    _form_wrap(
        [qw(text q), $q->param("q")],
        [qw(submit a search)],
    );

    print $out if $out;

    _html_footer();
}

sub action_inbox {
    $q->param("q", "in:inbox");
    return action_search();
}

sub action_search {
    my $data = _wave_request({
        id     => "op1",
        method => "wave.robot.search",
        params => {
            query => $q->param("q"),
        },
    });

    my $out = '';
    for my $digest (@{$data->{data}->{searchResults}->{digests}}) {
        $out .= q{<b>}.$digest->{title}.q{</b> }.$digest->{snippet}.q{<br />};
    }

    return $out;
}

# waveletdata
# {
#   'waveletId': 'eatenbyagrue.org!conv+root',
#   'waveId': 'eatenbyagrue.org!TBD_0x4c8cad8a',
#   'rootBlipId': 'TBD_eatenbyagrue.org!conv+root_0x401c33cd',
#   'participants': set(['rob@eatenbyagrue.org'])
# }
# blipdata
# {
#   'waveletId': 'eatenbyagrue.org!conv+root',
#   'blipId': 'TBD_eatenbyagrue.org!conv+root_0x401c33cd',
#   'waveId': 'eatenbyagrue.org!TBD_0x4c8cad8a',
#   'content': '',
#   'parentBlipId': None}
# }
#
# operation
# {
#   'id': 'op1'
#   'method': 'robot.createWavelet',
#   'params': {
#     'waveId': 'eatenbyagrue.org!TBD_0x16348be1'
#     'waveletId': 'eatenbyagrue.org!conv+root', 
#     'waveletData': {
#       'waveletId': 'eatenbyagrue.org!conv+root', 
#       'waveId': 'eatenbyagrue.org!TBD_0x16348be1',
#       'rootBlipId': 'TBD_eatenbyagrue.org!conv+root_0x4b666aa8',
#       'participants': [
#         'rob@eatenbyagrue.org'
#       ]
#     },
#   },
# }

sub action_test {
    my $wave_id = sprintf q{eatenbyagrue.org!TBD_0x%08x}, int rand 4294967296;
    my $wavelet_id = q{eatenbyagrue.org!conv+root};
    my $root_blip_id = sprintf q{TBD_%s_0x%08x}, $wavelet_id, int rand 4294967296;

    my $data = _wave_request({
        id => "test1",
        method => "wave.robot.createWavelet",
        params => {
            waveId => $wave_id,
            waveletId => $wavelet_id,
            waveletData => {
                waveId => $wave_id,
                waveletId => $wavelet_id,
                rootBlipId => $root_blip_id,
                participants => [
                    q{rob@eatenbyagrue.org},
                ],
            },
        },
    });

    return;
}

=pod
sub action_test {
    my $data = _wave_request({
        id => "test1",
        method => "wave.robot.fetchWave",
        params => {
            waveId => q{ga-staff-dev.monash.edu!w+Zgx9msiJA},
            waveletId => q{ga-staff-dev.monash.edu!conv+root},
        },
    });

    print $q->header("text/plain");
    print Dumper $data;
}
=cut

sub _wave_request {
    my ($rpc) = @_;

    my $oa_req = Net::OAuth->request("protected resource")->new(
        _default_request_params("POST"),
        request_url  => $rpc_uri,
        token        => $q->cookie("token"),
        token_secret => $q->cookie("secret"),
    );
    $oa_req->sign;

    my $ua = LWP::UserAgent->new;
    my $res = $ua->post($oa_req->to_url, Content_type => "application/json", Content => encode_json($rpc));

    if (!$res->is_success) {
        die "could not do rpc call: ".$res->status_line."\n".$res->content;
    }

    return decode_json($res->content);
}

sub _default_request_params {
    my ($method) = @_;
    $method //= "GET";

    return (
        consumer_key     => $consumer_key,
        consumer_secret  => $consumer_secret,
        request_method   => $method,
        signature_method => "HMAC-SHA1",
        timestamp        => time,
        nonce            => join('', rand_chars(size => 16, set => "alphanumeric"))
    );
}

sub _html_header {
    print $q->header("text/html");

    print <<HTML_HEADER
<html>
<head>
<title>ripple</title>
<style type="text/css">body { font-family: sans-serif; }</style>
</head>
<body>
HTML_HEADER
;
}

sub _html_footer {
    print <<HTML_FOOTER
</body>
</html>
HTML_FOOTER
;
}

sub _form_wrap {
    my (@elements) = @_;

    print q{<form action='}.$base_uri.q{' method='get'>};

    for my $element (@elements) {
        my ($type, $name, $value) = @$element;
        $value ||= '';
        print q{<input type='}.$type.q{' name='}.$name.q{' value='}.$value.q{' />};
    }

    print
        q{</form>}
    ;
}
