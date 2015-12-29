%%%-------------------------------------------------------------------
%%% @author Evgeniy Khramtsov <>
%%% @copyright (C) 2015, Evgeniy Khramtsov
%%% @doc
%%%
%%% @end
%%% Created : 20 Dec 2015 by Evgeniy Khramtsov <>
%%%-------------------------------------------------------------------
-module(ecrdt_dict).

%% API
-export([new/0, store/3, find/2, erase/2, to_list/1, from_list/1,
         merge/1, merge/2, is_dict/1, update_counter/3, size/1,
	 filter/2, fold/3, is_key/2, fetch/2, update/3, update/4,
	 fetch_keys/1]).

-include("ecrdt.hrl").

%%%===================================================================
%%% API
%%%===================================================================
new() ->
    #ecrdt_d{}.

is_dict(#ecrdt_d{}) -> true;
is_dict(_) -> false.

is_key(K, #ecrdt_d{add = A, rm = R}) ->
    case dict:find(K, A) of
	error -> false;
	{ok, {_, T}} -> not is_removed(K, R, T)
    end.

size(#ecrdt_d{add = A}) ->
    dict:size(A).

store(K, V, #ecrdt_d{add = A, rm = R}) ->
    #ecrdt_d{add = dict:store(K, {V, erlang:monotonic_time()}, A), rm = R}.

update(K, Fun, #ecrdt_d{add = A, rm = R}) ->
    NewA = dict:update(
	     K, fun({V, T}) ->
			case is_removed(K, R, T) of
			    true -> erlang:error(badarg);
			    false -> {Fun(V), erlang:monotonic_time()}
			end
		end, A),
    #ecrdt_d{add = NewA, rm = R}.

update(K, Fun, Initial, #ecrdt_d{add = A, rm = R}) ->
    NewA = dict:update(
	     K, fun({V, T}) ->
			case is_removed(K, R, T) of
			    true -> {Initial, erlang:monotonic_time()};
			    false -> {Fun(V), erlang:monotonic_time()}
			end
		end, {Initial, erlang:monotonic_time()}, A),
    #ecrdt_d{add = NewA, rm = R}.

find(K, #ecrdt_d{add = A, rm = R}) ->
    case dict:find(K, A) of
        error -> error;
        {ok, {V, T}} ->
            case is_removed(K, R, T) of
                true -> error;
                false -> {ok, V}
            end
    end.

fetch(K, #ecrdt_d{add = A, rm = R}) ->
    {V, T} = dict:fetch(K, A),
    case is_removed(K, R, T) of
	true -> erlang:error(badarg);
	false -> V
    end.

fetch_keys(Dict) ->
    fold(fun(K, _, Acc) -> [K|Acc] end, [], Dict).

erase(K, #ecrdt_d{add = A, rm = R}) ->
    #ecrdt_d{add = A, rm = dict:store(K, erlang:monotonic_time(), R)}.

to_list(#ecrdt_d{add = A, rm = R}) ->
    dict:fold(
      fun(K, {V, T}, KVs) ->
              case is_removed(K, R, T) of
                  true -> KVs;
                  false -> [{K, V}|KVs]
              end
      end, [], A).

from_list(KVs) ->
    lists:foldl(
      fun({K, V}, S) ->
              store(K, V, S)
      end, new(), KVs).

filter(Pred, Dict) ->
    R = dict:fold(
	  fun(Key, {Val, T}, Acc) ->
		  case is_removed(Key, Acc, T) of
		      true ->
			  Acc;
		      false ->
			  case Pred(Key, Val) of
			      true ->
				  Acc;
			      false ->
				  dict:store(Key, erlang:monotonic_time(), Acc)
			  end
		  end
	  end, Dict#ecrdt_d.rm, Dict#ecrdt_d.add),
    Dict#ecrdt_d{rm = R}.

fold(Fun, Acc0, #ecrdt_d{add = A, rm = R}) ->
    dict:fold(
      fun(Key, {Val, T}, Acc) ->
	      case is_removed(Key, R, T) of
		  true ->
		      Acc;
		  false ->
		      Fun(Key, Val, Acc)
	      end
      end, Acc0, A).

update_counter(_, 0, D) ->
    D;
update_counter(K, Incr, #ecrdt_d{add = A, rm = R}) ->
    Prev = case dict:find(K, A) of
               error -> ecrdt_counter:new();
               {ok, {C, T}} ->
                   case is_removed(K, R, T) of
                       true -> ecrdt_counter:new();
                       _ -> C
                   end
           end,
    New = ecrdt_counter:update(Prev, Incr),
    A1 = dict:store(K, {New, erlang:monotonic_time()}, A),
    #ecrdt_d{add = A1, rm = R}.

merge(#ecrdt_d{add = A1, rm = R1}, #ecrdt_d{add = A2, rm = R2}) ->
    #ecrdt_d{add = dict_union(A1, A2), rm = dict_union(R1, R2)}.

merge([]) ->
    erlang:error(badarg);
merge(Ss) ->
    hd(do_merge(Ss)).

%%%===================================================================
%%% Internal functions
%%%===================================================================
do_merge([S1, S2|Ss]) ->
    do_merge([merge(S1, S2)|Ss]);
do_merge(Ss) ->
    Ss.

dict_union(D1, D2) ->
    dict:merge(
      fun(_, {V1, T1}, {V2, T2}) ->
              T = lists:max([T1, T2]),
              V = ecrdt:merge(V1, V2),
              {V, T};
         (_, T1, T2) ->
              lists:max([T1, T2])
      end, D1, D2).

is_removed(Key, RSet, AddTime) ->
    case dict:find(Key, RSet) of
        {ok, RmvTime} when RmvTime >= AddTime ->
            true;
        _ ->
            false
    end.
