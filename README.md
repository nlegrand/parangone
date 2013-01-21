Parangone! is a concurrent benchmark tool made for fun. It's a
benchmarking tool in progress, you can safely ignore it :). It's not
very interessant, kind of my "first Erlang distributed service". This
README main goal is to remind me the way to start it up, sorry if it
doesn't make a lot of sense ^^.

    https://github.com/nlegrand/parangone.git
    cd parangone
    make

To communicate with the server we need a FQDN. If we test this on a
localhost let's do it :

    sudo hostname fully.qualified.dn
    ./parangone -n joe

The server is up, lets play with it from an erlang shell:

    erl -name will

We gonna use the http module which launch sequentially the same HTTP
request measuring the time it takes for each request. We need the
module intes to be started:

    (will@fully.qualified.dn)2> rpc:call(joe@fully.qualified.dn, inets, start, []).    
    ok

    will@fully.qualified.dn)6> rpc:call(joe@fully.qualified.dn, parangone, new, [sessionhttp, parangone_mod_http:get("http://localhost/"), 10]).
    {ok,sessionhttp}

This create a session called `sessionhttp` which we can retrieve from the ETS tables:

    (will@fully.qualified.dn)7> rpc:call(joe@fully.qualified.dn, parangone, get, [sessionhttp]).        {'joe@fully.qualified.dn',[{sessionhttp,[{1,200},
                                       {0,200},
                                       {0,200},
                                       {0,200},
                                       {1,200},
                                       {0,200},
                                       {0,200},
                                       {1,200},
                                       {0,200},
                                       {38,200}]}]}
