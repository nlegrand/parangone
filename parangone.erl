-module(parangone).
-export([http/1,start/0, rpc/2,timecount/1,stress/3,httpcount200/1]).

start() ->
    register(parangon, spawn(fun() -> loop() end)).

timecount(Fun) ->
    statistics(wall_clock),
    Result = Fun(),
    {_, Time} = statistics(wall_clock),
    {Time, Result}.

stress(_, 0, List)    ->
    List;
stress(Fun, Time, List) ->
    stress(Fun, Time - 1, [timecount(Fun)|List]).

rpc(Fun, Time) ->
    parangon ! {self(), Fun, Time},
    receive
        {parangon, Response} ->
            Response
    end.

loop() ->
    receive
        {From, Fun, Time} ->
            From ! {parangon, stress(Fun, Time, [])},
            loop()

    end.

http(Url) ->
    fun() ->
	    {_,{{_,Return,_},_,_}} = httpc:request(Url),
	    Return
    end.

httpcount200([]) ->
    0 ;
httpcount200([{Time, 200}|T]) ->
    Time + httpcount200(T).
