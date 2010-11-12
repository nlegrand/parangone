-module(parangone).
-export([http/1,start/0, rpc/2,timecount/1,stress/2,sort_response/1]).

start() ->
    register(parangon, spawn(fun() -> loop() end)).

timecount(Fun) ->
    statistics(wall_clock),
    Result = Fun(),
    {_, Time} = statistics(wall_clock),
    {Time, Result}.

stress(Fun, Time) ->
    stress(Fun, Time, []).

stress(_, 0, List) ->
    List;
stress(Fun, Time, List) ->
    stress(Fun, Time - 1, [timecount(Fun)|List]).

rpc(Fun, Time) ->
    parangon ! {self(), Fun, Time},
    receive
        {parangon, Node, Response} ->
            {Node, Response}
    end.

loop() ->
    receive
        {From, Fun, Time} ->
            From ! {parangon, node(), stress(Fun, Time, [])},
            loop()
    end.

http(Url) ->
    fun() ->
	    {_,{{_,Return,_},_,_}} = httpc:request(Url),
	    Return
    end.

sort_response(Response_List) ->
	       sort_response(Response_List, dict:new()).

sort_response([], Dict) ->
    dict:map(fun(_, List)
		-> { lists:sum(List) / length(List) }
	     end,
	     Dict);
sort_response([{Time, Code}|T], Dict) ->
    sort_response(T, dict:append(Code, Time, Dict)).

