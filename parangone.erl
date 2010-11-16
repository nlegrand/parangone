%%%-------------------------------------------------------------------
%%% @author Nicolas P. M. Legrand <nlegrand@ethelred.fr>
%%% @copyright (C) 2010, Nicolas P. M. Legrand
%%%
%%% Permission to use, copy, modify, and distribute this software for any
%%% purpose with or without fee is hereby granted, provided that the above
%%% copyright notice and this permission notice appear in all copies.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
%%%
%%% @doc
%%%
%%% @end
%%% Created : 15 Nov 2010 by Nicolas P. M. Legrand <nlegrand@ethelred.fr>
%%%-------------------------------------------------------------------
-module(parangone).

-behaviour(gen_server).

%% API
-export([start/0, stop/0, new/3, remove/1, get/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).
-compile(export_all).

-define(SERVER, ?MODULE). 

%-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Stops the server
%%
%% @spec stop() -> {ok, Dict}
%% @end
%%--------------------------------------------------------------------

stop() -> gen_server:call(?MODULE, stop).

new(Session, Fun, Time) -> gen_server:call(?MODULE, {new, Session, Fun, Time}).

remove(Session) ->
    gen_server:call(?MODULE, {remove, Session}).

remove_all() ->
    gen_server:call(?MODULE, remove_all).

all_sessions() ->
    gen_server:call(?MODULE, all_sessions).

get(Session) ->
    gen_server:call(?MODULE, {get, Session}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, Tab} |
%%                     {ok, Tab, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, ets:new(?MODULE,[])}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, Tab) ->
%%                                   {reply, Reply, Tab} |
%%                                   {reply, Reply, Tab, Timeout} |
%%                                   {noreply, Tab} |
%%                                   {noreply, Tab, Timeout} |
%%                                   {stop, Reason, Reply, Tab} |
%%                                   {stop, Reason, Tab}
%% @end
%%--------------------------------------------------------------------
handle_call({new, Session, Fun, Time}, _From, Tab) ->
    Reply = case ets:lookup(Tab, Session) of
		[] -> ets:insert(Tab, {Session, stress(Fun, Time)}),
		      {ok, Session};
		[_] -> {Session, already_exists}
	    end,
    {reply, Reply, Tab} ;
handle_call({remove, Session}, _From, Tab) ->
    Reply = case ets:lookup(Tab, Session) of
		[] -> {Session, does_not_exists};
		[{Session, _}] ->
		    ets:delete(Tab, Session),
		    {Session, removed}
	    end,
    {reply, Reply, Tab};
handle_call(all_sessions, _From, Tab) ->
    {reply, get_tab_keys(Tab), Tab};
handle_call(remove_all, _From, Tab) ->
    ets:delete(Tab),
    {reply, all_removed, ets:new(?MODULE,[])};
handle_call({get, Session}, _From, Tab) ->
    Reply = case ets:lookup(Tab, Session) of
		[] -> {Session, does_not_exists};
		[{Session, _}] ->
		    ets:lookup(Tab, Session)
	    end,
    {reply, Reply, Tab};
handle_call(stop, _From, Tab) ->
    {stop, normal, stopped, Tab}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, Tab) -> {noreply, Tab} |
%%                                  {noreply, Tab, Timeout} |
%%                                  {stop, Reason, Tab}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, Tab) ->
    {noreply, Tab}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, Tab) -> {noreply, Tab} |
%%                                   {noreply, Tab, Timeout} |
%%                                   {stop, Reason, Tab}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, Tab) ->
    {noreply, Tab}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, Tab) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _Tab) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, Tab, Extra) -> {ok, NewTab}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, Tab, _Extra) ->
    {ok, Tab}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

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

%sort_response(Response_List) ->
%	       sort_response(Response_List, dict:new()).

sort_response([], Dict) ->
    dict:map(fun(_, List)
		-> { lists:sum(List) / length(List) }
	     end,
	     Dict);
sort_response([{Time, Code}|T], Dict) ->
    sort_response(T, dict:append(Code, Time, Dict)).

get_tab_keys(Tab) ->
    get_tab_keys(Tab, ets:first(Tab), []).

get_tab_keys(Tab, Key, ListOfKeys) ->
    case Key of
	'$end_of_table' -> ListOfKeys;
	_ -> get_tab_keys(Tab, ets:next(Tab, Key), [Key] ++ ListOfKeys)
    end.
