#!/usr/bin/env perl

# ripple - pure-html wave client
# Robert Norris <rob@eatenbyagrue.org> - June 2010
# Artistic license 2.0: http://www.opensource.org/licenses/artistic-license-2.0.php

use 5.010;

use warnings;
use strict;

use URI::Escape;
use CGI::Carp qw(fatalsToBrowser);
use Net::OAuth 0.25;
use Data::Random qw(rand_chars);
use LWP::UserAgent;
use CGI ();
use JSON qw(decode_json encode_json);
use HTML::Entities;
use Data::Dumper;
use File::Basename;
use Data::Compare ();

# uri to the script. you can hard code this if you like, otherwise this will try to infer it
my $base_uri = sprintf "http://%s%s%s", $ENV{SERVER_NAME}, ($ENV{SERVER_PORT} == 80 ? q{} : ":$ENV{SERVER_PORT}"), $ENV{SCRIPT_NAME};

# path to splash screen file
my $splash_file = (fileparse($ENV{SCRIPT_FILENAME}))[1]."splash.html";

# url path to stylesheet
my $css_path = ($ENV{SCRIPT_NAME} =~ m{^(.*/)})[0] . "ripple.css";

# uri path to icons
my $icon_path = ($ENV{SCRIPT_NAME} =~ m{^(.*/)})[0] . "icons/";

# icons for attachments. key is the mime type, value is the file under $icon_path
my %icon_type_map = (
    '_unknown'   => 'unknown.png',
);

# oauth key and secret. if you change these you'll need to register your app with Google
my $consumer_key    = "anonymous";
my $consumer_secret = "anonymous";


# you shouldn't need to change anything under here

my $oa_scope = q{http://wave.googleusercontent.com/api/rpc};

my $oa_req_uri    = q{https://www.google.com/accounts/OAuthGetRequestToken?scope=}.uri_escape($oa_scope);
my $oa_auth_uri   = q{https://www.google.com/accounts/OAuthAuthorizeToken};
my $oa_access_uri = q{https://www.google.com/accounts/OAuthGetAccessToken};

my $rpc_uri = q{https://www-opensocial.googleusercontent.com/api/rpc};
my $sandbox_rpc_uri = q{https://www-opensocial-sandbox.googleusercontent.com/api/rpc};


local $Data::Dumper::Sortkeys = sub { my ($hash) = @_; return [sort { $a <=> $b } keys %$hash] };

my $q = CGI->new;

if ($q->param("l")) {
    do_wave();
    exit 0;
}

given ($q->param("s")) {
    when ("login") {
        do_login();
    }
    when ("callback") {
        do_callback();
    }
    when ("logout") {
        do_logout();
    }
    default {
        if ($q->cookie("token") && $q->cookie("secret")) {
            do_wave();
        } else {
            do_splash();
        }
    }
}

exit 0;

sub do_splash {
    print
        $q->header,

        _html_header(),

        _html_splash(),

        _form_wrap(
            [qw(submit s login)],
        ),

        _html_footer();
}

sub do_login {
    my $oa_req = Net::OAuth->request("request token")->new(
        _default_request_params(),
        request_url => $oa_req_uri,
        extra_params => {
            scope => $oa_scope,
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
        token    => $oa_res->token,
        callback => _build_internal_uri(s => 'callback'),
    );


    print $q->redirect(
        -uri => $oa_req->to_url($oa_auth_uri),
        -cookie => [
            $q->cookie(-name => "secret", -value => $oa_res->token_secret),
        ]
    );
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

    print $q->redirect(
        -uri => _build_internal_uri(), 
        -cookie => [
            $q->cookie(-name => "token",    -value => $oa_res->token),
            $q->cookie(-name => "secret",   -value => $oa_res->token_secret),
            $q->cookie(-name => "identity", -value => _identify_user($oa_res->token, $oa_res->token_secret)),
        ]
    );
}

sub do_logout {
    print $q->redirect(
        -uri => _build_internal_uri(), 
        -cookie => [
            $q->cookie(-name => "token",    -value => "", -expires => "-1d"),
            $q->cookie(-name => "secret",   -value => "", -expires => "-1d"),
            $q->cookie(-name => "identity", -value => "", -expires => "-1d"),
        ]
    );
}

sub do_wave {
    my %action_handler = (
        inbox    => \&action_inbox,
        search   => \&action_search,
        read     => \&action_read,
        redirect => \&action_redirect,
        reply    => \&action_reply,
        new      => \&action_new,
        add      => \&action_add,
    );

    my $out = '';

    my $action = $q->param("a") || "inbox";
    if (exists $action_handler{$action}) {
        $out = $action_handler{$action}->();
    }

    if (defined $out) {
        print
            $q->header("text/html"),

            _html_header(),

            q{<div class='identity'>},
                q{Logged in as: <b>}.$q->cookie("identity").q{</b>},
            q{</div>},

            _form_wrap(
                [qw(submit a inbox)],
                [qw(submit a new)],
                [qw(submit s logout)],
            ),

            q{<div class='search-box'>},
                _form_wrap(
                    [qw(text q), $q->param("q") || "in:inbox" ],
                    [qw(submit a search)],
                ),
            q{</div>},

            $out,

            _html_footer();
    }
}

sub action_inbox {
    $q->param("q", "in:inbox");
    return action_search();
}

sub action_search {
    my $data = _wave_request({
        id     => "search1",
        method => "wave.robot.search",
        params => {
            query      => $q->param("q"),
            index      => $q->param("i") // 0,
            numResults => 10,
        },
    });

    my $out;

    if ($data->{data}->{searchResults}->{numResults} == 0) {
        $out =
            q{<p>aww, no more...</p>};
    }

    else {
        $out = '';
        for my $digest (@{$data->{data}->{searchResults}->{digests}}) {
            my $title   = $digest->{title}   || "(no title)";
            my $snippet = $digest->{snippet} || "";

            $out .=
                q{<div class='search-item'>}.
                    q{<a href='}._build_internal_uri(a => 'read', w => $digest->{waveId}).q{'>}.
                        q{<h1>}.encode_entities($title).q{</h1>}.
                        encode_entities($snippet).
                    q{</a>}.
                q{</div>};
        }

        $out .= _form_wrap(
            [qw(hidden q), $q->param("q")],
            [qw(hidden i), ($q->param("i") // 0) + 10],
            [qw(hidden a search)],
            [q{submit}, undef, q{find more...}],
        );
    }

    if ($q->param("d")) {
        $out .=
            q{<div class='protocol-debug'>}.
                q{<pre>}.
                    encode_entities(Dumper($data)).
                q{</pre>}.
            q{</div>};
    }

    return $out;
}

sub action_read {
    my $wave_id = $q->param("w"); $wave_id =~ s/ /+/g;
    my ($wavelet_id) = $wave_id =~ m/^([^!]+)/;
    $wavelet_id .= q{!conv+root};

    my $data = _wave_request({
        id     => "read1",
        method => "wave.robot.fetchWave",
        params => {
            waveId    => $wave_id,
            waveletId => $wavelet_id,
        },
    });

    if ($data->{error}) {
        my $out =
            q{<p>}.
                q{<b>Error loading wave:</b><br />}.
                q{<code>}.$data->{error}->{message}.q{</code>}.
            q{</p>};

        if ($data->{error}->{message} =~ m/is not a participant/) {
            $out .=
                q{<p>}.
                    q{Note: There's currently a bug on Google's side that }.
                    q{sometimes stops you seeing waves you're not an }.
                    q{explicit participant in. Google are aware of the bug }.
                    q{and should have a fix available soon. Sorry!}.
                q{</p>}
        }

        return $out;
    }

    my $wave = ripple::wavelet->new({ data => $data->{data}, debug => $q->param("d") });
    return $wave->render;
}

sub action_redirect {
    print $q->redirect(-uri => $q->param("u"));
    return;
}

sub action_reply {
    my $wave_id        = $q->param("w");
    my $wavelet_id     = $q->param("wl");
    my $parent_blip_id = $q->param("b");

    my $blip_id = sprintf q{TBD_%s_0x%08x}, $wavelet_id, int rand 4294967296;

    my $data = _wave_request([{
        id     => "create1",
        method => "wave.blip.createChild",
        params => {
            waveId    => $wave_id,
            waveletId => $wavelet_id,
            blipId    => $parent_blip_id,
            blipData  => {
                waveId       => $wave_id,
                waveletId    => $wavelet_id,
                blipId       => $blip_id,
                parentBlipId => $parent_blip_id,
                content      => '',
            },
        },
    }, {
        id     => "append1",
        method => "wave.document.modify",
        params => {
            waveId       => $wave_id,
            waveletId    => $wavelet_id,
            blipId       => $blip_id,
            modifyAction => {
                modifyHow => "REPLACE",
                values    => [
                    "\n".$q->param("r"),
                ],
            },
        },
    }]);

    print $q->redirect(-uri => _build_internal_uri(a => 'read', w => $wave_id));
}

sub action_new {
    my $title = $q->param("t");

    if (!$title) {
        return
            q{<h1>create new wave</h1>}.
            q{<form class='new-wave-form' action='}._build_internal_uri().q{' method='post'>}.
                q{<p>}.
                    q{Wave title:<br />}.
                    q{<input type='text' name='t' />}.
                q{</p>}.
                q{<p>}.
                    q{Text:<br />}.
                    q{<textarea name='c'></textarea>}.
                q{</p>}.
                q{<input type='hidden' name='a' value='new' />}.
                q{<input type='submit' value='create new wave' />}.
            q{</form>};
    }

    my $content = $q->param("c");
 
    my ($domain) = $q->cookie("identity") =~ m/@(.*)$/;

    my $wave_id = sprintf q{%s!TBD_0x%08x}, $domain, int rand 4294967296;
    my $wavelet_id = sprintf q{%s!conv+root}, $domain;
    my $root_blip_id = sprintf q{TBD_%s_0x%08x}, $wavelet_id, int rand 4294967296;

    my $data = _wave_request([{
        id => "create1",
        method => "wave.robot.createWavelet",
        params => {
            waveId => $wave_id,
            waveletId => $wavelet_id,
            waveletData => {
                waveId => $wave_id,
                waveletId => $wavelet_id,
                rootBlipId => $root_blip_id,
                participants => [
                    $q->cookie("identity"),
                ],
            },
        },
    }, {
        id     => "insert1",
        method => "wave.document.modify",
        params => {
            waveId       => $wave_id,
            waveletId    => $wavelet_id,
            blipId       => $root_blip_id,
            modifyAction => {
                modifyHow => "INSERT",
                values    => [
                    "\n".$q->param("c"),
                ],
            },
        },
    }, {
        id     => "title1",
        method => "wave.wavelet.setTitle",
        params => {
            waveId       => $wave_id,
            waveletId    => $wavelet_id,
            waveletTitle => $q->param("t"),
        },
    }]);

    my $new_wave_id = $data->[0]->{data}->{waveId};
    print $q->redirect(-uri => _build_internal_uri(a => 'read', w => $new_wave_id));

    return;
}

sub action_add {
    if (!$q->param("r")) {
        return
            q{<h1>add recipients</h1>}.
            q{<form class='add-recipients-form' action='}._build_internal_uri().q{' method='post'>}.
                q{<p>}.
                    q{Enter addresses (one per line):<br />}.
                    q{<textarea name='r'></textarea>}.
                q{</p>}.
                q{<input type='hidden' name='a' value='add' />}.
                q{<input type='hidden' name='w' value='}.$q->param("w").q{' />}.
                q{<input type='hidden' name='wl' value='}.$q->param("wl").q{' />}.
                q{<input type='submit' value='add people' />}.
            q{</form>};
    }

    my $wave_id = $q->param("w");
    my $wavelet_id = $q->param("wl");

    my @recipients = split /[\r\n]+/, $q->param("r");

    my @ops;
    for my $i (0 .. $#recipients) {
        push @ops, {
            id     => "add".($i+1),
            method => "wave.wavelet.participant.add",
            params => {
                waveId        => $wave_id,
                waveletId     => $wavelet_id,
                participantId => $recipients[$i],
            }
        };
    }

    my $data = _wave_request(\@ops);

    print $q->redirect(-uri => _build_internal_uri(a => 'read', w => $wave_id));

    return;
}

sub _wave_request {
    my ($rpc, $opts) = @_;
    $opts //= {};

    if ($q->param("l")) {
        my $ops = ref $rpc eq "HASH" ? [$rpc] : $rpc;
        my @data;
        for my $id (map { $_->{id} } @$ops) {
            my $file = (fileparse($ENV{SCRIPT_FILENAME}))[1].q{l/}.$id;
            {
                no strict 'vars';
                push @data, eval do { (@ARGV, $/) = ($file); <> };
                die "error reading $file: $!" if $!;
                die $@ if $@;
            }
        }
        return ref $rpc eq "HASH" ? shift @data : \@data;
    }

    my $oa_req = Net::OAuth->request("protected resource")->new(
        _default_request_params("POST"),
        request_url  => $q->cookie("identity") =~ m/\@wavesandbox.com$/ ? $sandbox_rpc_uri : $rpc_uri,
        token        => $opts->{token}  // $q->cookie("token"),
        token_secret => $opts->{secret} // $q->cookie("secret"),
    );
    $oa_req->sign;

    my $ua = LWP::UserAgent->new;
    $ua->default_header(Authorization => $oa_req->to_authorization_header);
    my $res = $ua->post($oa_req->request_url, Content_type => "application/json", Content => encode_json($rpc));

    if (!$res->is_success) {
        die "could not do rpc call: ".$res->status_line."\n".$res->content;
    }

    my $data = decode_json($res->content);

#    if (ref $rpc eq "HASH" && $rpc->{id} eq "read1") {
#        _save_raw_data($rpc->{params}->{waveId}, $data);
#    }
    
    return $data;
}

# a utility function that I use from time to time to save the json locally so
# I can hack on ripple when I don't have network
sub _save_raw_data {
    my ($name, $data) = @_;

    my $filename = (fileparse($ENV{SCRIPT_FILENAME}))[1].q{l/}.$name;

    if (open my $fh, ">", $filename) {
        print $fh Dumper $data;
        close $fh;
    }
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

sub _build_internal_uri {
    my (%args) = @_;

    $args{d} = 1 if $q->param("d");

    my $fragment = delete $args{'#'};

    return $base_uri . (keys %args ? q{?}.join(q{&}, map { "$_=$args{$_}" } keys %args) : q{}) . ($fragment ? '#'.$fragment : q{});
}

sub _identify_user {
    my ($token, $secret) = @_;

    # the data api doesn't currently give us a way to get the identity
    # of the current user, so instead we use a hilarious hack discovered
    # by antimatter15 for microwave: try to fetch a wave we know we can't
    # access, and then extract our name from the error message
    #
    # this happens to be a convenient place to set the protocol version too
    
    my $wave_id    = "googlewave.com!w+bWEBb5mBA";
    my $wavelet_id = "googlewave.com!conv+nothing";

    my $data = _wave_request([{
        id     => "caps1",
        method => "wave.robot.notifyCapabilitiesHash",
        params => {
            protocolVersion => "0.21",
        },
    }, {
        id     => "read1",
        method => "wave.robot.fetchWave",
        params => {
            waveId    => $wave_id,
            waveletId => $wavelet_id,
        },
    }], {
        token  => $token,
        secret => $secret,
    });

    if ($data->[1]->{error}) {
        my ($identity) = $data->[1]->{error}->{message} =~ m/(\S+) is not a participant/;
        return $identity if $identity;
    }

    die "couldn't determine user identity";
}

sub _form_wrap {
    my @elements = grep { ref $_ eq "ARRAY" } @_;
    my ($opts)   = grep { ref $_ eq "HASH"  } @_;

    $opts //= {};
    $opts->{'method'} ||= 'get';

    my $out = q{<form action='}._build_internal_uri().q{' method='}.$opts->{method}.q{'>};

    push @elements, [qw(hidden d 1)] if $q->param("d");

    for my $element (@elements) {
        my ($type, $name, $value) = @$element;
        $value ||= '';
        if ($type eq 'textarea') {
            $out .=
                q{<textarea}.
                ($name ? q{ name='}.$name.q{'} : q{}).
                q{>}.$value.q{</textarea>};
        }
        else {
            $out .=
                q{<input type='}.$type.q{'}.
                ($name ? q{ name='}.$name.q{'} : q{}).
                q{ value='}.$value.q{' />};
        }
    }

    $out .= q{</form>};
}

sub _pretty_name {
    my ($name) = @_;
    $name =~ s{\@googlewave.com$}{};
    return $name;
}

sub _pretty_names {
    return map { _pretty_name($_) } @_;
}

sub _reply_textarea {
    my ($wave_id, $wavelet_id, $blip_id) = @_;

    return
        q{<div class='blip-reply'>}.
            _form_wrap( { method => 'post' },
               [qw(hidden w),  $wave_id],
               [qw(hidden wl), $wavelet_id],
               [qw(hidden b),  $blip_id],
               [qw(textarea r)],
               [qw(submit a reply)], 
            ).
        q{</div>}
    ;
}

sub _html_header {
    return <<HTML_HEADER
<html>
<head>
<title>ripple</title>
<link rel='stylesheet' type='text/css' href='$css_path' />
</head>
<body>
HTML_HEADER
;
}

sub _html_footer {
     return <<HTML_FOOTER
<p>
<a href='http://eatenbyagrue.org/a/ripple'>ripple</a> &copy; 2010 <a href='mailto:rob\@eatenbyagrue.org'>Robert Norris</a>
</p>

</body>
</html>
HTML_FOOTER
;
}

sub _html_splash {
    my $html = eval {
        do { local (@ARGV, $/) = ($splash_file); <ARGV> };
    };
    if (!$html) {
        $html = <<HTML_SPLASH
<h1>ripple</h1>

<p>
A pure-HTML client for <a href='http://wave.google.com/'>Google Wave</a>.
</p>
HTML_SPLASH
;
    }

    return $html;
}


package ripple::wavelet;

use base qw(Class::Accessor);

use HTML::Entities;
use Data::Dumper;

BEGIN {
    __PACKAGE__->mk_accessors(qw(data debug wave_id wavelet_id));
}

sub new {
    my ($class, $args) = @_;

    my $self = $class->SUPER::new($args);

    $self->wave_id($self->data->{waveletData}->{waveId});
    $self->wavelet_id($self->data->{waveletData}->{waveletId});

    return $self;
}

sub render {
    my ($self) = @_;

    my $data = $self->data;

    my $out =
        q{<div class='wave'>}.
            q{<div class='wave-action-box'>}.
                main::_form_wrap(
                    [qw(hidden w), $self->wave_id],
                    [qw(hidden wl), $self->wavelet_id],
                    [qw(hidden a add)],
                    [qw(submit), undef, q{add people}],
                ).
            q{</div>}.
            q{In this wave: }.
            q{<b>}.join(q{</b>, <b>}, main::_pretty_names(@{$data->{waveletData}->{participants}})).q{</b>}.
        q{</div>};

    $out .= $self->blip($data->{waveletData}->{rootBlipId})->render;

    if ($self->debug) {
        $out .=
            q{<div class='protocol-debug'>}.
                q{<pre>}.
                    encode_entities(Dumper($data)).
                q{</pre>}.
            q{</div>};
    }

    return $out;
}

sub blip {
    my ($self, $blip_id) = @_;

    my $blipdata = $self->data->{blips}->{$blip_id};
    return if not $blipdata;

    return ripple::blip->new({ wavelet => $self, data => $blipdata });
}



package ripple::blip;

use base qw(Class::Accessor);

use Date::Format;
use HTML::Entities;

BEGIN {
    __PACKAGE__->mk_accessors(qw(wavelet data blip_id));
}

sub new {
    my ($class, $args) = @_;

    my $self = $class->SUPER::new($args);

    $self->blip_id($self->data->{blipId});

    return $self;
}

sub render {
    my ($self) = @_;

    my $data = $self->data;

    my %children = map { $_ => 1 } @{$data->{childBlipIds}};
    delete $children{$_} for map { $_->{type} eq "INLINE_BLIP" ? $_->{properties}->{id} : () } values %{$data->{elements}};

    my $out;

    my $distance = 0;
    {
        my $blip = $self;
        while ($blip) {
            $blip = $self->wavelet->blip($blip->data->{parentBlipId});
            $distance++ if $blip;
        }
    }

    $out .=
        q{<div class='blip' id='}.$self->blip_id.q{'>}.
            q{<b>}.main::_pretty_name($data->{creator}).q{</b>}.
            time2str(q{ at <b>%l:%M%P</b> on <b>%e %B</b>}, $data->{lastModifiedTime}/1000);

    my @contributors = main::_pretty_names(grep { $_ ne $data->{creator} } @{$data->{contributors}});
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
                q{distance: }.$distance.q{<br />}.
            q{</div>};
    }

    my @element_positions = sort { $a <=> $b } keys %{$data->{elements}};
    for my $i (0 .. $#element_positions) {
        my $position = $element_positions[$i];
        my $element = $data->{elements}->{$position};

        given ($element->{type}) {
            when ("LINE") {
                push @{$self->{elements}}, ripple::line->new({
                    blip       => $self,
                    start      => $position,
                    end        => $i == $#element_positions ? length $data->{content} : $element_positions[$i+1],
                    properties => $element->{properties},
                });
            }
            default {
                push @{$self->{elements}}, ripple::element->new({
                    blip       => $self,
                    position   => $position,
                    type       => $element->{type},
                    properties => $element->{properties},
                });
            }
        }
    }

    for my $annotation (@{$data->{annotations}}) {
        push @{$self->{annotations}}, ripple::annotation->new({
            blip  => $self,
            start => $annotation->{range}->{start},
            end   => $annotation->{range}->{end},
            name  => $annotation->{name},
            value => $annotation->{value},
        });
    }

    $out .= q{<div class='blip-content'>};

    my $linegroup = ripple::linegroup->new({ renderer => $self });

    for my $element (@{$self->{elements}}) {
        if ($element->isa("ripple::line")) {
            if (! $linegroup->add($element)) {
                $out .= $linegroup->render;
                $linegroup = ripple::linegroup->new({ renderer => $self });

                $linegroup->add($element);
            }
        }
        else {
            $out .= $linegroup->render;
            $linegroup = ripple::linegroup->new({ renderer => $self });

            $out .= $element->render;
        }
    }

    $out .= $linegroup->render;

    $out .= q{</div>};

    #
    # that's the blip rendered. now for the stuff under it. this happens
    # slightly differently depending on what kind of blip it is.
    #
    # since we don't have access to the conversation model, we have to infer
    # things a bit. this results it a structure that isn't strictly correct
    # but there's limits to what we can do
    #
    # we define three blip types: root, top and thread, as follows
    #
    # root
    # top
    #   thread
    #   thread
    #   thread
    # top
    #   thread
    # top
    # top
    #   thread
    #   thread
    #
    # replies to thread blips appear underneath as more thread blips (this is
    # where the lack of the conversation model hurts). a reply box is added to
    # the root and every top blip
    #
    # inline blips are just top blips rendered inside the blip content
    #
    # the distance is the how far away we are from the root. because of the
    # way children are defined are setup in the wave json, we can infer the
    # following from the blip distance:
    #
    # 0: root
    # 1: top
    # 2+ thread
    #

    # root blip gets a reply box
    $out .= main::_reply_textarea($self->wavelet->wave_id, $self->wavelet->wavelet_id, $self->blip_id) if $distance == 0;

    # the root and thread blips don't have any other blips inside them
    $out .= q{</div>} if $distance != 1;

    # render the child blips
    if (@{$data->{childBlipIds}}) {
        for my $child_blip_id (grep { exists $children{$_} } @{$data->{childBlipIds}}) {
            $out .= $self->wavelet->blip($child_blip_id)->render;
        }
    }

    # end of a top blip
    if ($distance == 1) {
        # get a reply box after all their thread blips.  the reply gets added
        # to the final thread blip though. more conversation model hack
        $out .= main::_reply_textarea($self->wavelet->wave_id, $self->wavelet->wavelet_id, @{$data->{childBlipIds}} ? $data->{childBlipIds}->[-1] : $self->blip_id);

        # and that's that
        $out .= q{</div>} if $distance == 1;
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

    # find the annotations
    my @annotations = grep {
        ($_->start >= $start && $_->start < $end) || ($_->end > $start && $_->end <= $end) || ($_->start < $start && $_->end > $end)
    } @{$self->{annotations}};

    # if there's no annotations then the raw content is all we need
    if (!@annotations) {
        return $self->content_range($start, $end);
    }

    # loop over the annotations and figure out where all the boundaries are.
    # while we're at it, build a list of annotations with coerced ranges such
    # that they always fall inside the wanted range
    my %boundaries;
    my @coerced_annotations;
    for my $annotation (@annotations) {
        my $boundary_start = $annotation->start < $start ? $start : $annotation->start;
        my $boundary_end   = $annotation->end   > $end   ? $end   : $annotation->end;

        push @coerced_annotations, ripple::annotation->new({
            start => $boundary_start,
            end   => $boundary_end,
            name  => $annotation->name,
            value => $annotation->value,
        });

        $boundaries{$boundary_start} = [];
        $boundaries{$boundary_end}   = [];
    }

    # loop over the boundary positions and attach a list of split annotations
    # that start at that position to each
    my @positions = sort { $a <=> $b } keys %boundaries;
    for my $i (0 .. $#positions-1) {
        my $position = $positions[$i];

        my @start_annotations = grep { $_->start <= $position && $_->end > $position } @coerced_annotations;

        for my $annotation (@start_annotations) {
            my $split_annotation = ripple::annotation->new({
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
                my $marker = $annotation->boundary_marker;

                push @elems, $_ for @{$marker->{elements}};
                $style += keys %{$marker->{style}};
            }

            $content .= q{</}.$_.q{>} for map { $_->{tag} } reverse @elems;
            $content .= q{</span>} if $style;
        }

        if (@start_annotations) {
            my (@elems, %style);

            for my $annotation (@start_annotations) {
                my $marker = $annotation->boundary_marker;

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



package ripple::line;

use base qw(Class::Accessor);

BEGIN {
    __PACKAGE__->mk_accessors(qw(blip start end properties));
}

sub render {
    my ($self) = @_;

    my $out = '';

    #$out = sprintf q{<pre>LINE [%d %d]: %s</pre>}, $self->start, $self->end, Data::Dumper::Dumper($self->properties);

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

    return $out;
}



package ripple::annotation;

use base qw(Class::Accessor);

use URI::Escape;

BEGIN {
    __PACKAGE__->mk_accessors(qw(blip start end name value));
}

sub boundary_marker {
    my ($self) = @_;

    my $marker = {};

    given ($self->name) {

        when (m{^style/(.*)}) {
            my $name = $1;
            $name =~ s/([A-Z])/q{-}.lc($1)/e;
            $marker->{style}->{$name} = $self->value;
        }

        when (m{^link/(?:manual|auto)}) {
            my $href;

            if (my ($waveid) = $self->value =~ m{^waveid://(.*)}) {
                $waveid =~ s{/}{!};
                $href = main::_build_internal_uri(a => 'read', w => uri_escape($waveid));
            }
            else {
                $href = main::_build_internal_uri(a => 'redirect', u => uri_escape($self->value));
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



package ripple::linegroup;

use base qw(Class::Accessor);

BEGIN {
    __PACKAGE__->mk_accessors(qw(blip properties _div_close));
}

sub add {
    my ($self, $line) = @_;

    # if this is the first object, just add it
    if ($self->count == 0) {
        $self->_add_internal($line);
        return 1;
    }

    # if its exactly the same as us, we can add it
    if (Data::Compare::Compare($self->properties, $line->properties)) {
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
    $self->{subgroup} = ripple::linegroup->new({ blip => $self->blip });
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

sub render {
    my ($self) = @_;

    if (exists $self->{subgroup}) {
        $self->_add_internal($self->{subgroup});
        delete $self->{subgroup};
    }

    return '' if !exists $self->{objects};

    my $props = $self->properties;

    my $out = '';

    given ($props->{lineType}) {
        when ("li") {
            $out .= q{<ul>};
        }
        default {
            my (@class, @style);

            push @class, "indent" if $props->{indent};

            push @style, "direction: rtl" if exists $props->{direction} && $props->{direction} eq 'r';

            push @style, "text-align: ".($props->{alignment} eq 'c' ? "center"  :
                                         $props->{alignment} eq 'r' ? "right"   :
                                         $props->{alignment} eq 'j' ? "justify" :
                                                                      "left") if exists $props->{alignment} && $props->{alignment} ne 'l';

            if (@class || @style) {
                $out .= q{<div};
                if (@class) {
                    $out .= q{ class='}.join(' ', @class).q{'};
                }
                if (@style) {
                    $out .= q{ style='}.join('; ', @style).q{;'};
                }
                $out .= q{>};

                $self->_div_close(1);
            }
        }
    }

    for my $object (@{$self->{objects}}) {
        $out .= $object->render;
    }

    given ($props->{lineType}) {
        when ("li") {
            $out .= q{</ul>};
        }
        default {
            if ($self->_div_close) {
                $out .= q{</div>};
                $self->_div_close(0);
            }
        }
    }

    #$out .= sprintf q{<pre>LINEGROUP [%d]: %s</pre>}, $self->count, Data::Dumper::Dumper($self->properties);

    return $out;
}



package ripple::element;

use base qw(Class::Accessor);

BEGIN {
    __PACKAGE__->mk_accessors(qw(blip position type properties));
}

sub new {
    my ($class, $args) = @_;

    my $type = delete $args->{type};

    given ($type) {
        when (m/^(?:IMAGE|ATTACHMENT)$/) {
            return ripple::attachment->new($args);
        }
        when ("INLINE_BLIP") {
            return ripple::inline_blip->new($args);
        }
        when ("GADGET") {
            return ripple::gadget->new($args);
        }
        default {
            return $class->SUPER::new({%$args, type => $type});
        }
    }
}

sub render {
    my ($self) = @_;

    return $q->pre(q{ELEMENT }.$self->type);
}



package ripple::attachment;

use base qw(ripple::element);

sub render {
    my ($self) = @_;

    my $props = $self->properties;

    my $caption = $props->{caption} ? $props->{caption} : $props->{attachmentId};
    my $icon = $icon_type_map{$props->{mimeType}} ? $icon_type_map{$props->{mimeType}} : $icon_type_map{_unknown};

    my $type = !exists $props->{mimeType} || $props->{mimeType} =~ m{^image/(?:png|gif|jpeg)$} ? "image" : "attachment";

    my $url = $props->{attachmentUrl} || $props->{url};

    my $out =
        q{<div class='}.$type.q{'>}.
            q{<a href='}.$url.q{'}.
                q{<img}.
                    q{ src='}.($type eq "image" ? $url : $icon_path.$icon).q{'}.
                    q{ alt='}.$caption.q{'}.
                q{ />}.
                q{<br />}.
                $caption.
            q{</a>}.
        q{</div>};

    return $out;
}



package ripple::inline_blip;

use base qw(ripple::element);

sub render {
    my ($self) = @_;

    return $self->blip->wavelet->blip($self->properties->{id})->render;
}



package ripple::gadget;

use base qw(ripple::element);

use HTML::Entities;
use Data::Dumper;

sub new {
    my ($class, $args) = @_;

    if ($class ne __PACKAGE__) {
        return $class->SUPER::new($args);
    }

    # XXX this should be a global
    my %gadget_classes = (
        "http://wave-api.appspot.com/public/gadgets/areyouin/gadget.xml" => "ripple::gadget::yesnomaybe",
    );

    if (my $target_class = $gadget_classes{$args->{properties}->{url}}) {
        return $target_class->new($args);
    }

    return $class->SUPER::new($args);
}

sub render {
    my ($self) = @_;

    my $props = $self->properties;

    my $out =
        q{<div class='gadget-unknown'>}.
            q{GADGET: }.encode_entities($props->{url}).
        q{</div>};

    if ($self->blip->wavelet->debug) {
        $out .=
            q{<div class='protocol-debug'>}.
                q{<pre>}.
                    encode_entities(Dumper($props)).
                q{</pre>}.
            q{</div>};
    }

    return $out;
}



package ripple::gadget::yesnomaybe;

use base qw(ripple::gadget);

sub render {
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
                $user;

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

