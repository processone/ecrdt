%%%-------------------------------------------------------------------
%%% @author Evgeniy Khramtsov <>
%%% @copyright (C) 2015, Evgeniy Khramtsov
%%% @doc
%%%
%%% @end
%%% Created : 20 Dec 2015 by Evgeniy Khramtsov <>
%%%-------------------------------------------------------------------
-record(ecrdt_d, {add = dict:new() :: dict:dict(),
		  rm  = dict:new() :: dict:dict()}).

-record(ecrdt_c, {pos = [] :: [{node(), integer()}],
		  neg = [] :: [{node(), integer()}]}).

-record(ecrdt_s, {add = dict:new() :: dict:dict(),
		  rm  = dict:new() :: dict:dict()}).
