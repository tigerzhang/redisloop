%%%-------------------------------------------------------------------
%%% @author zhanghu
%%% @copyright (C) 2015, yunba.io
%%% @doc
%%% 基于 zookeeper 的网络管理实现
%%% @end
%%% Created : 24. 十一月 2015 下午7:18
%%%-------------------------------------------------------------------
-module(cluster_impl_zk).
-author("zhanghu").

-define(CLUSTER, "/redisloop/cluster").

%% API
-export([init/1, connect/1, watch_cluster/4, take_owner/2, watch_loop/2]).

init(_Config) ->
    ok.

connect(_Config) ->
    erlzk:connect([{"localhost", 2181}], 30000).

watch_loop(Conn, Pid) ->
    receive
        {Method, Path, Event} when Event == node_data_changed; Event == node_created ->
            lager:info("event ~p ~n", [{Method, Path, Event}]),
            {ok, {Data, _}} = erlzk:get_data(Conn, Path, spawn(cluster_impl_zk, watch_loop, [Conn, Pid])),
            {ok, Slot, Replica} = path_to_slot_replica(Path),
            gen_server:cast(Pid, {Event, Slot, Replica, Data}),
            lager:info("Data ~p~n", [binary_to_list(Data)]);
        {Method, Path, Event} when Event == node_deleted ->
            lager:info("event ~p ~n", [{Method, Path, Event}]),
            erlzk:exists(Conn, Path, spawn(cluster_impl_zk, watch_loop, [Conn, Pid])),
            gen_server:cast(Pid, {Event, Path});
        {Method, Path, Event} when Event == node_children_changed ->
            lager:info("event ~p ~n", [{Method, Path, Event}]),
            erlzk:get_children(Conn, Path, spawn(cluster_impl_zk, watch_loop, [Conn, Pid])),
            gen_server:cast(Pid, {Event, Path});
        Other ->
            lager:info("event ~p", [Other])
    end.


watch_cluster(Conn, Slot, Replica, Pid) ->
    Cluster = ?CLUSTER,

    lager:info("watch ~p~n", [Cluster]),

    Watcher = spawn(cluster_impl_zk, watch_loop, [Conn, Pid]),

    erlzk:exists(Conn, Cluster, Watcher),
    erlzk:get_children(Conn, Cluster, Watcher),
    erlzk:get_data(Conn, Cluster ++ "/" ++ integer_to_list(Slot) ++ "/" ++ integer_to_list(Replica), Watcher),
    ok.

take_owner(Conn, {Slot, Replica}) ->
    lager:info("take_owner ~p ~p", [?MODULE, take_owner]),

    Path = ?CLUSTER ++ "/" ++ integer_to_list(Slot) ++ "/" ++ integer_to_list(Replica),

    erlzk:set_data(Conn, Path, term_to_binary(node())),
    ok.

path_to_slot_replica(Path) ->
    PathBin = list_to_binary(Path),
    case binary_split(PathBin, <<"/">>, [global]) of
        [<<>>, _, _, Slot, Replica] ->
            {ok, binary_to_integer(Slot), binary_to_integer(Replica)};
        _ ->
            error
    end.