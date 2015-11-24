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
-export([init/1, connect/1, watch_cluster/1, take_owner/2]).

init(_Config) ->
    ok.

connect(_Config) ->
    erlzk:connect([{"localhost", 2181}], 30000).

watch_cluster(Conn) ->
    Cluster = ?CLUSTER,

    lager:info("watch ~p~n", [Cluster]),

    Watcher = spawn(fun() ->
        receive
        % receive a node deleted event
            {Event, Path} ->
                Path = Cluster,
                Event = node_deleted
        end
                   end),
    erlzk:get_data(Conn, Cluster, Watcher),
    ok.

take_owner(Conn, {Slot, Replica}) ->
    Path = ?CLUSTER ++ "/" ++ integer_to_list(Slot) ++ "/" ++ integer_to_list(Replica),

    erlzk:set_data(Conn, Path, term_to_binary(node())),
    ok.
