%%%-------------------------------------------------------------------
%%% @author Evgeniy Khramtsov <>
%%% @copyright (C) 2015, Evgeniy Khramtsov
%%% @doc
%%%
%%% @end
%%% Created : 20 Dec 2015 by Evgeniy Khramtsov <>
%%%-------------------------------------------------------------------
-module(ecrdt_counter).

%% API
-export([new/0, incr/1, decr/1, value/1, merge/1, is_counter/1, merge/2,
         update/2, update/3]).

-include("ecrdt.hrl").

-opaque counter() :: #ecrdt_c{}.

-export_type([counter/0]).

%%%===================================================================
%%% API
%%%===================================================================
-spec new() -> counter().
new() ->
    #ecrdt_c{}.

-spec is_counter(term()) -> boolean().
is_counter(#ecrdt_c{}) -> true;
is_counter(_) -> false.

-spec incr(counter()) -> counter().
incr(Counter) ->
    update(node(), Counter, 1).

-spec decr(counter()) -> counter().
decr(Counter) ->
    update(node(), Counter, -1).

-spec update(counter(), integer()) -> counter().
update(Counter, Incr) ->
    update(node(), Counter, Incr).

-spec value(counter()) -> integer().
value(#ecrdt_c{pos = Pos, neg = Neg}) ->
    lists:sum([N || {_, N} <- Pos]) - lists:sum([N || {_, N} <- Neg]).

-spec merge([counter()]) -> counter().
merge([]) ->
    erlang:error(badarg);
merge(Cs) ->
    hd(do_merge(Cs)).

-spec merge(counter(), counter()) -> counter().
merge(#ecrdt_c{pos = P1, neg = N1}, #ecrdt_c{pos = P2, neg = N2}) ->
    #ecrdt_c{pos = merge2(P1, P2), neg = merge2(N1, N2)}.

-spec update(node(), counter(), integer()) -> counter().
update(Node, #ecrdt_c{pos = Pos, neg = Neg}, Incr) ->
    if Incr > 0 ->
            #ecrdt_c{pos = do_update(Node, Pos, Incr), neg = Neg};
       Incr < 0 ->
            #ecrdt_c{pos = Pos, neg = do_update(Node, Neg, -1*Incr)};
       Incr == 0 ->
            #ecrdt_c{pos = Pos, neg = Neg}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
do_update(Node, Counter, Incr) ->
    case lists:keytake(Node, 1, Counter) of
        {value, {_, N}, Counter1} ->
            [{Node, N+Incr}|Counter1];
        false ->
            [{Node, Incr}|Counter]
    end.

do_merge([C1, C2|Cs]) ->
    do_merge([merge(C1, C2)|Cs]);
do_merge(Cs) ->
    Cs.

merge2([{Node, V1}|C1], C) ->
    case lists:keytake(Node, 1, C) of
        {value, {_, V2}, C2} ->
            [{Node, lists:max([V1, V2])}|merge2(C1, C2)];
        false ->
            [{Node, V1}|merge2(C1, C)]
    end;
merge2([], C2) ->
    C2;
merge2(C1, []) ->
    C1.
