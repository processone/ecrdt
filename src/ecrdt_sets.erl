%%%-------------------------------------------------------------------
%%% @author Evgeniy Khramtsov <>
%%% @copyright (C) 2015, Evgeniy Khramtsov
%%% @doc
%%%
%%% @end
%%% Created : 20 Dec 2015 by Evgeniy Khramtsov <>
%%%-------------------------------------------------------------------
-module(ecrdt_sets).

%% API
-export([new/0, is_set/1, size/1, add_element/2, del_element/2,
         is_element/2, to_list/1, from_list/1, merge/1, merge/2,
         subtract/2, union/2, value/1]).

-include("ecrdt.hrl").

-opaque ecrdt_set() :: #ecrdt_s{}.
-export_type([ecrdt_set/0]).

%%%===================================================================
%%% API
%%%===================================================================
-spec new() -> ecrdt_set().
new() ->
    #ecrdt_s{}.

-spec is_set(term()) -> boolean().
is_set(#ecrdt_s{}) -> true;
is_set(_) -> false.

-spec size(ecrdt_set()) -> non_neg_integer().
size(#ecrdt_s{add = A, rm = R}) ->
    dict:fold(
      fun(E, TA, Size) ->
              case dict:find(E, R) of
                  {ok, TR} when TR >= TA ->
                      Size;
                  _ ->
                      Size+1
              end
      end, 0, A).

-spec add_element(term(), ecrdt_set()) -> ecrdt_set().
add_element(E, #ecrdt_s{add = A, rm = R}) ->
    #ecrdt_s{add = dict:store(E, erlang:monotonic_time(), A), rm = R}.

-spec del_element(term(), ecrdt_set()) -> ecrdt_set().
del_element(E, #ecrdt_s{add = A, rm = R}) ->
    #ecrdt_s{add = A, rm = dict:store(E, erlang:monotonic_time(), R)}.

-spec is_element(term(), ecrdt_set()) -> boolean().
is_element(E, #ecrdt_s{add = A, rm = R}) ->
    case dict:find(E, A) of
        error ->
            false;
        {ok, TA} ->
            case dict:find(E, R) of
                error -> true;
                {ok, TR} -> TA > TR
            end
    end.

-spec to_list(ecrdt_set()) -> list().
to_list(#ecrdt_s{add = A, rm = R}) ->
    dict:fold(
      fun(E, TA, Es) ->
              case dict:find(E, R) of
                  {ok, TR} when TR >= TA ->
                      Es;
                  _ ->
                      [E|Es]
              end
      end, [], A).

-spec from_list(list()) -> ecrdt_set().
from_list(Es) ->
    lists:foldl(
      fun(E, S) ->
              add_element(E, S)
      end, new(), Es).

-spec merge(ecrdt_set(), ecrdt_set()) -> ecrdt_set().
merge(#ecrdt_s{add = A1, rm = R1}, #ecrdt_s{add = A2, rm = R2}) ->
    #ecrdt_s{add = dict_union(A1, A2), rm = dict_union(R1, R2)}.

-spec subtract(ecrdt_set(), ecrdt_set()) -> ecrdt_set().
subtract(#ecrdt_s{add = A1, rm = R1}, #ecrdt_s{add = A2}) ->
    #ecrdt_s{add = A1, rm = dict_union(R1, A2)}.

-spec merge([ecrdt_set()]) -> ecrdt_set().
merge([]) ->
    erlang:error(badarg);
merge(Ss) ->
    hd(do_merge(Ss)).

-spec union(ecrdt_set(), ecrdt_set()) -> ecrdt_set().
union(S1, S2) ->
    merge(S1, S2).

-spec value(ecrdt_set()) -> list().
value(S) ->
    to_list(S).

%%%===================================================================
%%% Internal functions
%%%===================================================================
do_merge([S1, S2|Ss]) ->
    do_merge([merge(S1, S2)|Ss]);
do_merge(Ss) ->
    Ss.

dict_union(D1, D2) ->
    dict:merge(fun(_, T1, T2) -> lists:max([T1, T2]) end, D1, D2).
