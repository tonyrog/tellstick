-module(tellstick_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Options) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Options).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(Options) ->
    Server = {tellstick_server, {tellstick_server, start_link, [Options]},
	      permanent, 5000, worker, [tellstick_server]},
    {ok, { {one_for_one,3,5}, [Server]} }.


