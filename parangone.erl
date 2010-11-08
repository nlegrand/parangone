-module(parangone).
-export([http/1,stress/2,loop/0,start/0, rpc/2]).

start() ->
    register(parangon, spawn(fun() -> loop() end)).

http(Url) ->
    statistics(wall_clock),
    http:request("http://" ++ Url),
    {_, Time} = statistics(wall_clock),
    Time.

stress(Url, 1)    ->
    http(Url);
stress(Url, Time) ->
    http(Url) + stress(Url, Time - 1).

rpc(Request, Time) ->
    parangon ! {self(), Request, Time},
    receive
        {parangon, Response} ->
            Response
    end.


loop() ->
    receive
        {From, Url, Time} ->
            From ! {parangon, stress(Url, Time) / Time},
            loop()

    end.

    
