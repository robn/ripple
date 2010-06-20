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
use Date::Format;
use Data::Dumper;
use File::Basename;

# uri to the script. you can hard code this if you like, otherwise this will try to infer it
my $base_uri = sprintf "http://%s%s%s", $ENV{SERVER_NAME}, ($ENV{SERVER_PORT} == 80 ? q{} : ":$ENV{SERVER_PORT}"), $ENV{SCRIPT_NAME};

# path to splash screen file
my $splash_file = (fileparse($ENV{SCRIPT_FILENAME}))[1]."splash.html";

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

    print $q->redirect(-uri => _build_internal_uri(), -cookie => [$token_cookie, $secret_cookie]);
}

sub do_logout {
    my $token_cookie = $q->cookie(-name => "token", -value => "");
    my $secret_cookie = $q->cookie(-name => "secret", -value => "");

    print $q->redirect(-uri => _build_internal_uri(), -cookie => [$token_cookie, $secret_cookie]);
}

sub do_wave {
    my %action_handler = (
        inbox    => \&action_inbox,
        search   => \&action_search,
        read     => \&action_read,
        redirect => \&action_redirect,
        reply    => \&action_reply,
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

            _form_wrap(
                [qw(submit a inbox)],
                [qw(submit s logout)],
            ),

            q{<div class='search-box'>},
                _form_wrap(
                    [qw(text q), $q->param("q")],
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
            $out .=
                q{<div class='search-item'>}.
                    q{<a href='}._build_internal_uri(a => 'read', w => $digest->{waveId}).q{'>}.
                        q{<h1>}.encode_entities($digest->{title}).q{</h1>}.
                        encode_entities($digest->{snippet}).
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
                    Dumper($data).
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

    my $out;

    if ($data->{error}) {
        $out .=
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

    my $root_blip_id = $data->{data}->{waveletData}->{rootBlipId};
    $out = _render_blip($wave_id, $wavelet_id, $root_blip_id, $data->{data}->{blips});

    if ($q->param("d")) {
        $out .=
            q{<div class='protocol-debug'>}.
                q{<pre>}.
                    Dumper($data).
                q{</pre>}.
            q{</div>};
    }

    return $out;
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

sub _wave_request {
    my ($rpc) = @_;

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
        request_url  => $rpc_uri,
        token        => $q->cookie("token"),
        token_secret => $q->cookie("secret"),
    );
    $oa_req->sign;

    my $ua = LWP::UserAgent->new;
    $ua->default_header(Authorization => $oa_req->to_authorization_header);
    my $res = $ua->post($oa_req->request_url, Content_type => "application/json", Content => encode_json($rpc));

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

sub _build_internal_uri {
    my (%args) = @_;

    $args{d} = 1 if $q->param("d");

    my $fragment = delete $args{'#'};

    return $base_uri . (keys %args ? q{?}.join(q{&}, map { "$_=$args{$_}" } keys %args) : q{}) . ($fragment ? '#'.$fragment : q{});
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

sub _render_blip {
    my ($wave_id, $wavelet_id, $blip_id, $blips, $distance) = @_;
    $distance ||= 0;

    my $blip = $blips->{$blip_id};

    my %children = map { $_ => 1 } @{$blip->{childBlipIds}};
    delete $children{$_} for map { $_->{type} eq "INLINE_BLIP" ? $_->{properties}->{id} : () } values %{$blip->{elements}};

    my $out = '';
    $out .=
        q{<div class='blip' id='}.$blip_id.q{'>}.
        q{<b>}.$blip->{creator}.q{</b>}.
        time2str(q{ at <b>%l:%M%P</b> on <b>%e %B</b>}, $blip->{lastModifiedTime}/1000);

    my @contributors = grep { $_ ne $blip->{creator} } @{$blip->{contributors}};
    if (@contributors) {
        $out .=
            q{<br />}.
            q{with <b>}.
            join (q{</b>, <b>}, @contributors).
            q{</b>};
    }

    if ($q->param("d")) {
        $out .= 
            q{<div class='blip-debug'>}.
                q{blip: }.$blip_id.q{<br />}.
                q{parent: }.$blip->{parentBlipId}.q{<br />}.
            q{</div>};
    }

    $out .= q{<div class='blip-content'>};

    # 0 - end annotation
    # 1 - element
    # 2 - start annotation

    my $blipmeta = {};
    for my $pos (keys %{$blip->{elements}}) {
        push @{$blipmeta->{$pos}}, [ 1, $blip->{elements}->{$pos} ];
    }
    for my $annotation (@{$blip->{annotations}}) {
        # certain elements render strangely if rendered "inside" some ranges
        # (eg an inline blip inside an link range). to get around this we
        # check each annotation to see if there is one of these elements
        # inside it. if there is, we create two or more annotations such that
        # they sit between each element in the range
        my @element_positions = grep { $_ > $annotation->{range}->{start} && 
                                       $_ < $annotation->{range}->{end}   &&
                                       $blip->{elements}->{$_}->{type} =~ m/^(?:INLINE_BLIP|ATTACHMENT|GADGET)$/ } keys %{$blip->{elements}};

        push @{$blipmeta->{$annotation->{range}->{start}}}, [ 2, $annotation ];
        for my $element_position (@element_positions) {
            push @{$blipmeta->{$element_position}},         [ 0, $annotation ];
            push @{$blipmeta->{$element_position}},         [ 2, $annotation ];
        }
        push @{$blipmeta->{$annotation->{range}->{end}}},   [ 0, $annotation ];
    }

    if (!keys %$blipmeta) {
        $out .= $blip->{content};
    }

    else {
        my @positions = sort { $a <=> $b } keys %$blipmeta;
        for my $i (0 .. $#positions) {
            my $position = $positions[$i];

            my (%start, %end, %point);

            for my $meta (sort { $a <=> $b } @{$blipmeta->{$position}}) {
                my ($type, $thing) = @$meta;

                if ($type == 2) {
                    given ($thing->{name}) {
                        when ("conv/title") {
                            push @{$start{elements}}, [qw(h1)];
                        }

                        when (m{^link/(?:manual|auto)}) {
                            next if $start{link};
                            $start{link} = _build_internal_uri(a => 'redirect', u => uri_escape($thing->{value}));
                        }
                        when ("link/wave") {
                            next if $start{link};
                            $start{link} = _build_internal_uri(a => 'read', w => $thing->{value});
                        }

                        when (m{^style/(.*)}) {
                            my $name = $1;
                            $name =~ s/([A-Z])/q{-}.lc($1)/e;
                            $start{style}->{$name} = $thing->{value};
                        }

                        default {
                            #$out .= q{<span style='background-color: #000066; color: #ffffff;'>}.$thing->{name}.q{</span>};
                        }
                    }

                }

                elsif ($type == 0) {
                    given ($thing->{name}) {
                        when ("conv/title") {
                            push @{$end{elements}}, qw(h1);
                        }

                        when (m{^link/}) {
                            $end{link}++;
                        }

                        when (m{^style/}) {
                            $end{style}++;
                        }

                        default {
                            #$out .= q{<span style='background-color: #006600; color: #ffffff;'>}.$thing->{name}.q{</span>};
                        }
                    }

                }

                elsif ($type == 1) {
                    given ($thing->{type}) {
                        when ("LINE") {
                            push @{$point{elements}}, [q{br}] if $positions[$i] != 0;
                        }
                        when ("INLINE_BLIP") {
                            push @{$point{blips}}, $thing->{properties}->{id};
                        }
                        when ("ATTACHMENT") {
                            push @{$point{attachments}}, $thing->{properties};
                        }
                        when ("GADGET") {
                            push @{$point{gadgets}}, $thing->{properties};
                        }
                        default {
                            #$out .= q{<span style='background-color: #660000; color: #ffffff;'>}.$thing->{type}.q{</span>};
                        }
                    }
                }
            }

            # range end
            if ($end{style}) {
                $out .= q{</span>} x $end{style};
            }

            if ($end{link}) {
                $out .= q{</a>} x $end{link};
            }

            for my $elem (@{$end{elements}}) {
                $out .= q{</}.$elem.q{>};
            }

            # points
            for my $elem (@{$point{elements}}) {
                my ($tag, %attrs) = @$elem;
                $out .= q{<}.$tag;
                $out .= q{ }.$_.q{='}.$attrs{$_}.q{'} for keys %attrs;
                $out .= q{ />};
            }

            $out .= _render_attachment($_) for @{$point{attachments}};
            $out .= _render_gadget($_) for @{$point{gadgets}};
            $out .= _render_blip($wave_id, $wavelet_id, $_, $blips, 1) for @{$point{blips}};

            # range start
            for my $elem (@{$start{elements}}) {
                my ($tag, %attrs) = @$elem;
                $out .= q{<}.$tag;
                $out .= q{ }.$_.q{='}.$attrs{$_}.q{'} for keys %attrs;
                $out .= q{>};
            }

            if ($start{link}) {
                $out .= q{<a href='}.$start{link}.q{'>};
            }

            if (keys %{$start{style}}) {
                $out .= q{<span style='};
                $out .= $_.q{: }.$start{style}->{$_}.q{;} for keys %{$start{style}};
                $out .= q{'>};
            }


            # blip content
            #$out .= q{<span style='background-color: #000000; color: #ffffff'>}.$position.q{ - }.($i < $#positions ? $positions[$i+1] : length $blip->{content}).q{</span>};

            $out .= encode_entities(substr ($blip->{content},
                                            $position,
                                            ($i < $#positions ? $positions[$i+1] : length $blip->{content}) - $position));
        }
    }

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
    $out .= _reply_textarea($wave_id, $wavelet_id, $blip_id) if $distance == 0;

    # the root and thread blips don't have any other blips inside them
    $out .= q{</div>} if $distance != 1;

    # render the child blips
    if (@{$blip->{childBlipIds}}) {
        for my $child_blip_id (grep { exists $children{$_} } @{$blip->{childBlipIds}}) {
            $out .= _render_blip($wave_id, $wavelet_id, $child_blip_id, $blips, $distance+1);
        }
    }

    # end of a top blip
    if ($distance == 1) {
        # get a reply box after all their thread blips.  the reply gets added
        # to the final thread blip though. more conversation model hack
        $out .= _reply_textarea($wave_id, $wavelet_id, @{$blip->{childBlipIds}} ? $blip->{childBlipIds}->[-1] : $blip_id);

        # and that's that
        $out .= q{</div>} if $distance == 1;
    }

    return $out;
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

sub _render_attachment {
    my ($properties) = @_;

    my $caption = $properties->{caption} ? $properties->{caption} : $properties->{attachmentId};
    my $icon = $icon_type_map{$properties->{mimeType}} ? $icon_type_map{$properties->{mimeType}} : $icon_type_map{_unknown};

    my $type = $properties->{mimeType} =~ m{^image/(?:png|gif|jpeg)$} ? "image" : "attachment";

    my $out =
        q{<div class='}.$type.q{'>}.
            q{<a href='}.$properties->{attachmentUrl}.q{'}.
                q{<img}.
                    q{ src='}.($type eq "image" ? $properties->{attachmentUrl} : $icon_path.$icon).q{'}.
                    q{ alt='}.$caption.q{'}.
                q{ />}.
                q{<br />}.
                $caption.
            q{</a>}.
        q{</div>};

    return $out;
}

sub _render_gadget {
    my ($properties) = @_;

    my $out =
        q{<div class='gadget'>}.
            q{GADGET: }.encode_entities($properties->{url});

    if ($q->param("d")) {
        $out .=
            q{<div class='protocol-debug'>}.
                q{<pre>}.
                    Dumper($properties).
                q{</pre>}.
            q{</div>};
    }

    $out .=
        q{</div>};

    return $out;
}

sub _html_header {
    return <<HTML_HEADER
<html>
<head>
<title>ripple</title>
<style type="text/css">
body {
    font-family: sans-serif;
    font-size: smaller;
}

img {
    border: none;
}

div.search-box {
    padding: 0 5px 0 5px;
}
div.search-box input[type=text] {
    width: 100%;
}
div.search-box input[type=submit] {
    position: absolute;
    right: 10px;
}

div.search-item {
    margin: 2px;
    padding: 2px;
    border: solid black 1px;
    background-color: #ffff99;
}
div.search-item:hover {
    background-color: #999999;
}
div.search-item h1 {
    margin: 0;
    padding: 0;
    font-size: larger;
}
div.search-item > a {
    display: block;
    color: inherit;
    text-decoration: none;
}

/* root blip */
body > div.blip {
    margin: 5px;
    padding: 5px;
    background-color: #9999ff;
    border: solid black 1px;
}

/* normal blip */
div.blip > div.blip {
    margin: 5px;
    padding: 5px;
    background-color: #9999ff;
}

/* inline blip */
div.blip-content > div.blip {
    margin: 5px;
    padding: 5px;
    background-color: #9999ff;
    border: solid black 1px;
}

div.blip-debug {
    float: right;
    margin-right: 5px;
    padding: 2px;
    border: solid black 1px;
    background-color: #ffff99;
    font-family: monospace;
}

div.blip-content {
    padding: 5px;
    background-color: #ffffff;
    border: solid black 1px;
}
div.blip-content h1 {
    display: inline;
}

div.blip-reply {
    padding: 5px;
}
div.blip-reply form {
    display: inline;
}
div.blip-reply textarea {
    width: 100%;
}

div.image, div.attachment {
    display: table-cell;
    border: solid #999999 1px;
    background-color: #ffff99;
    margin: 0;
    padding: 0;
}
div.image > a, div.attachment > a {
    display: block;
    padding: 5px;
    color: inherit;
    text-decoration: none;
}

div.gadget {
    border: dashed #666666 3px;
    background-color: #99ff99;
    font-family: monospace;
    padding: 2px;
}

div.protocol-debug {
    border: solid black 1px;
    background-color: #cccccc;
}
div.protocol-debug > pre {
    margin: 5px;
}
</style>
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

