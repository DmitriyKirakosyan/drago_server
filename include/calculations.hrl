%%%-------------------------------------------------------------------
%%% @author dima
%%% @copyright (C) 2016, nooneknows
%%% @doc
%%%
%%% @end
%%% Created : 01. Nov 2016 10:58 PM
%%%-------------------------------------------------------------------
-author("dima").


-type stonetype() :: 1 | 2 | undefined.
-type board() :: list(list()).

-define (OUT_OF_BOARD, undefined).
-define (EMPTY, 0).
-define (BLACK, 1).
-define (WHITE, 2).


-record(group, {
    stone_type = undefined :: stonetype(),
    stones = [] :: list(),
    isAlive = false :: boolean() | undefined
}).

-record(zone, {
    stones = [] :: list(),
    contactWith = [] :: list(),
    belongsTo = undefined :: stonetype()
}).
