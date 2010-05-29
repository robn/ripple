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

my $consumer_key     = q{ga-staff-dev.monash.edu};
my $consumer_secret  = q{1tSn68zoE0iUJH/UWtf+EsbI};
my $private_key_file = q{/home/rob/sso/google_ga-staff-dev.monash.edu.key};

my $scope = q{http://wave.googleusercontent.com/api/rpc};

my $oa_req_uri    = q{https://www.google.com/accounts/OAuthGetRequestToken?scope=}.uri_escape($scope);
my $oa_auth_uri   = q{https://www.google.com/accounts/OAuthAuthorizeToken};
my $oa_access_uri = q{https://www.google.com/accounts/OAuthGetAccessToken};

my $rpc_uri = q{https://www-opensocial.googleusercontent.com/api/rpc};


my $private_key = Crypt::OpenSSL::RSA->new_private_key(do { local (@ARGV, $/) = ($private_key_file); <> });

my $base_uri = "http://junai/ripple/ripple.cgi";

my $q = CGI->new;
given ($q->param("do")) {
    when ("login") {
        do_login($q);
        exit 0;
    }
    when ("callback") {
        do_callback($q);
        exit 0;
    }
    when ("wave") {
        do_wave($q);
        exit 0;
    }
    default {
        if ($q->param("token")) {
            print $q->redirect("$base_uri?do=wave&token=".$q->param("token"));
        }
        else {
            print $q->redirect("$base_uri?do=login");
        }
        exit 0;
    }
}

sub do_login {
    my ($q) = @_;

    my $oa_req = Net::OAuth->request("request token")->new(
        _default_request_params(),
        request_url => $oa_req_uri,
        extra_params => {
            scope => $scope,
        },
    );
    $oa_req->sign($private_key);

    my $ua = LWP::UserAgent->new;
    my $res = $ua->get($oa_req->to_url);

    if (!$res->is_success) {
        die "could not get request token: ".$res->status_line."\n".$res->content;
    }

    my $oa_res = Net::OAuth->response("request token")->from_post_body($res->content);

    $oa_req = Net::OAuth->request("user auth")->new(
        token    => $oa_res->token,
        callback => "$base_uri?do=callback",
    );

    print $q->redirect($oa_req->to_url($oa_auth_uri));
}

sub do_callback {
    my ($q) = @_;

    my $oa_res = Net::OAuth->response("user auth")->from_hash({$q->Vars});

    my $oa_req = Net::OAuth->request("access token")->new(
        _default_request_params(),
        request_url  => $oa_access_uri,
        token        => $oa_res->token,
        token_secret => '',
    );
    $oa_req->sign($private_key);

    my $ua = LWP::UserAgent->new;
    my $res = $ua->get($oa_req->to_url);

    if (!$res->is_success) {
        die "could not get access token: ".$res->status_line."\n".$res->content;
    }

    my $oa_res = Net::OAuth->response("access token")->from_post_body($res->content);
    my $token = $oa_res->token;

    print $q->redirect("$base_uri?do=wave&token=$token");
}

sub do_wave {
    my ($q) = @_;

    given ($q->param("action")) {
        when ("inbox") {
            do_wave_inbox($q);
        }
        default {
            print $q->redirect("$base_uri?do=wave&token=".$q->param("token")."&action=inbox");
        }
    }
}

sub do_wave_inbox {
    my ($q) = @_;

    my $data = _wave_request($q, {
        id     => "op1",
        method => "wave.robot.search",
        params => {
            query => "in:inbox",
        },
    });

    print $q->header("text/plain");
    print Dumper $data;
}

sub _wave_request {
    my ($q, $rpc) = @_;

    my $oa_req = Net::OAuth->request("protected resource")->new(
        _default_request_params("POST"),
        request_url  => $rpc_uri,
        token        => $q->param("token"),
        token_secret => '',
    );
    $oa_req->sign($private_key);

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
        consumer_secret  => '',
        request_method   => $method,
        signature_method => "RSA-SHA1",
        timestamp        => time,
        nonce            => join('', rand_chars(size => 16, set => "alphanumeric"))
    );
}
