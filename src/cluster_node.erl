%%%-------------------------------------------------------------------
%%% @author zhanghu
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. 十一月 2015 下午7:11
%%%-------------------------------------------------------------------
-module(cluster_node).
-author("zhanghu").

-behaviour(gen_server).

%% API
-export([start_link/0, take_owner/2, create/2]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {cluster_conn, pre_owner, cur_owner}).

%%%===================================================================
%%% API
%%%===================================================================
create(Slot, Replica) ->
    NodeName = list_to_atom("node_" ++ integer_to_list(Slot) ++ "_" ++ integer_to_list(Replica)),
    lager:info("create ~p~n", [NodeName]),
    gen_server:start_link({local, NodeName}, ?SERVER, [Slot, Replica], []).

take_owner(Slot, Replica) ->
    gen_server:call(?SERVER, {take_owner, Slot, Replica}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([Slot, Replica]) ->
    {ok, Conn} = cluster_impl_zk:connect([]),
    cluster_impl_zk:watch_cluster(Conn, Slot, Replica, self()),
    {ok, #state{cluster_conn = Conn}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_call({take_owner, Slot, Replica}, _From, #state{cluster_conn = ClusterConn} = State) ->
    lager:debug("take_owner ~p ~p~n", [Slot, Replica]),
    Ret = cluster_impl_zk:take_owner(ClusterConn, {Slot, Replica}),
    {reply, Ret, State};
handle_call(_Request, _From, State) ->
    lager:error("Unhandled req ~p~n", [_Request]),
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
    lager:debug("handle_cast ~p~n", [_Request]),
    gen_server:cast(cluster_manager, _Request),
    case _Request of
        {node_data_changed, Path, Data} ->
            lager:info("~p owned by ~p~n", [Path, binary_to_term(Data)])
    end,
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
    {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
%% @doc
%% Join a cluster
%%
%% @spec
%% @end

join() ->
    ok.

leave() ->
    ok.