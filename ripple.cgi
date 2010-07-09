#!/usr/bin/env perl

# ripple - pure-html wave client
# Robert Norris <rob@eatenbyagrue.org> - June 2010
# Artistic license 2.0: http://www.opensource.org/licenses/artistic-license-2.0.php

use 5.010;

use warnings;
use strict;

use FindBin;
use lib "$FindBin::Bin/lib";

use App::Ripple;

use CGI::Carp qw(fatalsToBrowser);
use CGI ();
use HTML::Entities;
use Data::Dumper;
use File::Basename;

my $q = CGI->new;

my $base_uri = ($ENV{SCRIPT_NAME} =~ m{^(.*)/})[0];

my $app = App::Ripple->new({
    script_uri => $ENV{SCRIPT_NAME},
    readme_uri => "$base_uri/splash.html",
    css_uri    => "$base_uri/ripple.css",
    icon_uri   => "$base_uri/icons",

    debug      => $q->param("d"),
});

# oauth key and secret. if you change these you'll need to register your app with Google
my $oa_consumer_key    = "anonymous";
my $oa_consumer_secret = "anonymous";


# you shouldn't need to change anything under here

local $Data::Dumper::Sortkeys = sub { my ($hash) = @_; return [sort { $a <=> $b } keys %$hash] };


my $waveservice = App::Ripple::WaveService->new({
    consumer_key    => $oa_consumer_key,
    consumer_secret => $oa_consumer_secret,
});
$waveservice->use_sandbox($q->cookie("identity") =~ m/\@wavesandbox.com$/);

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
    print $q->redirect($app->readme_uri);
    exit 0;
}

sub do_login {
    my ($uri, $token_secret) = $waveservice->get_login_uri($app->build_internal_uri(s => 'callback'));

    print $q->redirect(
        -uri => $uri,
        -cookie => [
            $q->cookie(-name => "secret", -value => $token_secret),
        ]
    );
}

sub do_callback {
    my ($token, $token_secret) = $waveservice->handle_callback($q->cookie("secret"), {$q->Vars});

    print $q->redirect(
        -uri => $app->build_internal_uri(), 
        -cookie => [
            $q->cookie(-name => "token",    -value => $token),
            $q->cookie(-name => "secret",   -value => $token_secret),
            $q->cookie(-name => "identity", -value => _identify_user($token, $token_secret)),
        ],
    );
}

sub do_logout {
    print $q->redirect(
        -uri => $app->build_internal_uri(), 
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
                    q{<a href='}.$app->build_internal_uri(a => 'read', w => $digest->{waveId}).q{'>}.
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

    my $wavelet = App::Ripple::Wavelet->new({ app => $app, data => $data->{data}, debug => $q->param("d") });
    return $wavelet->render;
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

    my $reply_text = $q->param("r");
    $reply_text =~ s{\r\n}{\n}smg;
    $reply_text =~ s{\r}{}smg;

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
                    "\n".$reply_text,
                ],
            },
        },
    }]);

    print $q->redirect(-uri => $app->build_internal_uri(a => 'read', w => $wave_id));
}

sub action_new {
    my $title = $q->param("t");

    if (!$title) {
        return
            q{<h1>create new wave</h1>}.
            q{<form class='new-wave-form' action='}.$app->build_internal_uri().q{' method='post'>}.
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
    print $q->redirect(-uri => $app->build_internal_uri(a => 'read', w => $new_wave_id));

    return;
}

sub action_add {
    if (!$q->param("r")) {
        return
            q{<h1>add recipients</h1>}.
            q{<form class='add-recipients-form' action='}.$app->build_internal_uri().q{' method='post'>}.
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

    print $q->redirect(-uri => $app->build_internal_uri(a => 'read', w => $wave_id));

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

    return $waveservice->rpc_call($opts->{token} // $q->cookie("token"), $opts->{secret} // $q->cookie("secret"), $rpc);
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
            protocolVersion => "0.22",
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

    my $out = q{<form action='}.$app->build_internal_uri().q{' method='}.$opts->{method}.q{'>};

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

sub _html_header {
    return <<HTML_HEADER
<html>
<head>
<title>ripple</title>
<link rel='stylesheet' type='text/css' href='${\$app->css_uri}' />
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
