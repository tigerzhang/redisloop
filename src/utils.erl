%%%-------------------------------------------------------------------
%%% @author zhanghu
%%% @copyright (C) 2015, yunba.io
%%% @doc
%%%
%%% @end
%%% Created : 23. 十一月 2015 下午6:14
%%%-------------------------------------------------------------------
-module(utils).
-author("zhanghu").

-define(prof(Label), true).
-define(forp(Label), true).
-define(balance_prof, true).

-define(FNV_OFFSET_BASIS, 2166136261).
-define(FNV_PRIME,        16777619).

%% API
-export([hash/1]).

hash(Term) ->
    ?prof(hash),
    R = fnv(Term),
    ?forp(hash),
    R.

% 32 bit fnv. magic numbers ahoy
fnv(Term) when is_binary(Term) ->
    fnv_int(?FNV_OFFSET_BASIS, 0, Term);

fnv(Term) ->
    fnv_int(?FNV_OFFSET_BASIS, 0, term_to_binary(Term)).

fnv_int(Hash, ByteOffset, Bin) when erlang:byte_size(Bin) == ByteOffset ->
    Hash;

fnv_int(Hash, ByteOffset, Bin) ->
    <<_:ByteOffset/binary, Octet:8, _/binary>> = Bin,
    Xord = Hash bxor Octet,
    fnv_int((Xord * ?FNV_PRIME) rem (2 bsl 31), ByteOffset+1, Bin).
