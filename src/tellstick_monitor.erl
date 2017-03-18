%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2017, Tony Rogvall
%%% @doc
%%%    Simple tellstick monitor/logger
%%% @end
%%% Created : 26 Feb 2014 by Tony Rogvall <tony@rogvall.se>

-module(tellstick_monitor).

-behaviour(gen_server).

%% API
-export([start_link/0,start_link/1, stop/0]).
-export([start/0,start/1]).

%% gen_server callbacks
-export([init/1, 
	 handle_call/3, 
	 handle_cast/2, 
	 handle_info/2,
	 terminate/2, 
	 code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, 
	{
	  subscription
	}).


start_link() ->
    start_link([]).

start_link(Opts) ->
    gen_server:start_link({local,?SERVER}, ?MODULE, Opts, []).

start() ->
    start([]).

start(Opts) ->
    gen_server:start({local,?SERVER}, ?MODULE, Opts, []).

stop() ->
    gen_server:call(?SERVER, stop).

%%--------------------------------------------------------------------

init(_Opts) ->
    application:start(syntax_tools),
    application:start(compiler),
    application:start(goldrush),
    application:start(lager),
    application:start(uart),
    case tellstick:start() of
	ok -> 
	    {ok,Ref} = tellstick_server:subscribe(),
	    {ok,#state{ subscription=Ref }};
	{error,{already_started,_App}} -> 
	    {ok,Ref} = tellstick_server:subscribe(),
	    {ok,#state{ subscription=Ref }};
	{error,Reason} ->
	    {error, Reason}
    end.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Call, _From, State) ->
    lager:debug("handle_cast: Unknown call ~p", [_Call]),
    {reply, {error,bad_call}, State}.


handle_cast(_Msg, State) ->
    lager:debug("handle_cast: Unknown message ~p", [_Msg]),
    {noreply, State}.


handle_info({tellstick_event,Ref,Data},  State) 
  when Ref =:= State#state.subscription ->
    lager:info("event ~p", [Data]),
    {noreply, State};
handle_info(_Info, State) ->
    lager:debug("handle_info: Unknown info ~p", [_Info]),
    {noreply, State}.

terminate(_Reason, State) ->
    tellstick_server:unsubscribe(State#state.subscription),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
