%%%-------------------------------------------------------------------
%%% @author dima
%%% @copyright (C) 2016, nooneknows
%%% @doc
%%%
%%% @end
%%% Created : 01. Nov 2016 10:58 PM
%%%-------------------------------------------------------------------
-author("dima").

-include ("drago.hrl").


-type stonetype() :: 1 | 2 | undefined.
-type board() :: list(list()).
-type points() :: list(#point{}).

-define (OUT_OF_BOARD, undefined).
-define (EMPTY, 0).
-define (BLACK, 1).
-define (WHITE, 2).

-record(simple_stone, {
    type :: integer(),
    point :: #point{}
}).

-record (rect, {
    origin = #point{},
    width = 0 :: integer(),
    height = 0 :: integer()
}).

-record(group, {
    id :: integer(),
    stone_type = undefined :: stonetype(),
    stones = [] :: points(),
    connectedGroups = [] :: list(#group{}),
    rect :: #rect{},
    isAlive = false :: boolean() | undefined
}).

-record(zone, {
    points = [] :: list(),
    contactWith = [] :: list(),
    contactWithTypes = [] :: list(),
    belongsTo = undefined :: stonetype()
}).

-record(estimating, {
    board :: board(),
    zones :: list(#zone{}),
    bGroups :: list(#group{}),
    bCapturedGroups = [] :: list(#group{}),
    wGroups :: list(#group{}),
    wCapturedGroups = [] :: list(#group{})
}).