%%%-------------------------------------------------------------------
%%% @author zhanghu
%%% @copyright (C) 2015, yunba.io
%%% @doc
%%%
%%% @end
%%% Created : 19. 十一月 2015 下午7:32
%%%-------------------------------------------------------------------
-module(redis_proxy).
-author("zhanghu").

-behaviour(gen_server).

%% API
-export([start_link/0,
    info/0
    , rcmd/1]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).
-define(SLOTNUM, 1024).

-record(state, {c1, c2}).

%%
%% 一个 proxy 需要知道的信息：
%% － 全局路由
%%    所有的 slot 对应的 nodes
%% - 当前 node 维护的 slot
%% - 当前 node 是否处于 migration 状态; migration target/migration source
%%

%%%===================================================================
%%% API
%%%===================================================================
-spec(info() -> ok).
info() ->
    gen_server:call(?SERVER, info).

rcmd(Cmd) ->
    gen_server:call(?SERVER, {rcmd, Cmd}).

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
init([]) ->
    {ok, RedisStat1} = cluster_node:start(1, <<>>),
    {ok, RedisStat2} = cluster_node:start(2, <<>>),
    {state, C1, _, _, _, _, _, _} = RedisStat1,
    {state, C2, _, _, _, _, _, _} = RedisStat2,

    %% key -> slot -> node
    %% route table of slot -> node
    ets:new(slotroute, [set, named_table, public]),

    %% slot maintained by this node
    ets:new(slotowned, [set, named_table]),

    load_route(),
    {ok, #state{c1 = C1, c2 = C2}}.

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
handle_call({rcmd, {<<"SET">>, Key, Value}}, _From, #state{c1=C1} = State) ->
    Nodes = key_to_nodes(Key),

    %% 写请求分发到所有节点
    [gen_server:call(Node, {cmd, {<<"SET">>, Key, Value}}) || Node <- Nodes],

    %% 判断每一个节点的返回值，如果返回错误：
    %% 1. 路由错误：路由信息发生变化，写请求去了错误的路由，重新发请求给正确的 node
    %% 2. 响应超时、redis 出错：节点设置为不可用


    {reply, ok, State};
handle_call({cmd, Command}, _Front, #state{c1=C1} = State) ->
    Ret = hierdis:command(C1, Command),
    {reply, Ret, State};
handle_call(info, _From, #state{c1=C1, c2=C2} = State) ->
    Info1 = hierdis:command(C1, [<<"INFO">>]),
    Info2 = hierdis:command(C2, [<<"INFO">>]),
    io:format("~p ~p~n", [Info1, Info2]),
    {reply, ok, State};
handle_call(_Request, _From, State) ->
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
load_route() ->
    ok.

key_to_slot(Key) ->
    utils:hash(Key) rem ?SLOTNUM.

key_to_nodes(Key) ->
    slot_to_nodes(key_to_slot(Key)).

-spec slot_to_nodes(Slot ::integer()) -> [].
slot_to_nodes(Slot) ->
    ets:lookup(route, Slot).