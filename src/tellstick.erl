%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2014, Tony Rogvall
%%% @doc
%%%    Starter kit
%%% @end
%%% Created : 26 Feb 2014 by Tony Rogvall <tony@rogvall.se>

-module(tellstick).

-export([start/0]).

start() ->
    application:start(tellstick).

%% wrap tellstick_server calls ?

