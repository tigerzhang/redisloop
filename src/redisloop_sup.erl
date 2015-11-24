%%%-------------------------------------------------------------------
%% @doc redisloop top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module('redisloop_sup').

-behaviour(supervisor).

%% API
-export([start_link/0,
    start_child/0
]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child() ->
    io_lib:format("~p", ["start child"]),
    io_lib:format("~p", [supervisor:start_child(?SERVER, [])]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

init([]) ->
    {ok, { {one_for_all, 0, 1}, [
        ?CHILD(cluster_node, worker)
    ]} }.

%%====================================================================
%% Internal functions
%%====================================================================
