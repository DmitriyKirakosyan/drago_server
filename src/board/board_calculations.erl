-module (board_calculations).
-include_lib("eunit/include/eunit.hrl").

-export ([getDeadStones/2]).
-export([pointValue/2]).
-export([aroundPoints/1]).
-export([checkIfSubgroup/2]).
-export([isFirstLine/2]).

-include("calculations.hrl").

%% board = [[0, 0, 1, 1, 2, ..], ..]


%%%
%%% KILL or GET KILLED!
%%%
getDeadStones(Point, Board) ->
    getDeadStones(Board, pointValue(Point, Board), [], [Point]).

getDeadStones(_, ?EMPTY, _, _) -> [];
getDeadStones(_, ?OUT_OF_BOARD, _, _) -> [];
getDeadStones(_, _, DeadStones, []) -> DeadStones;
getDeadStones(Board, StoneType, DeadStones, [PointToConsider | FreshPoints]) ->
    case pointValue(PointToConsider, Board) of
        ?EMPTY -> []; %% no dead stones => return empty list
        StoneType ->
            %% fetch new near points
            NewPoints = lists:filter(
                fun(Point) -> not lists:member(Point, FreshPoints) andalso not lists:member(Point, DeadStones) end
            , aroundPoints(PointToConsider)),
            %% go further with updated info
            getDeadStones(Board, StoneType, [PointToConsider | DeadStones], FreshPoints ++ NewPoints);

        %% ?OUT_OF_BORDER or other type stone => just go further
        _ -> getDeadStones(Board, StoneType, DeadStones, FreshPoints)
    end.

aroundPoints(#point{x = X, y = Y} = Point) ->
    [
        Point#point{ y = Y + 1 },
        Point#point{ x = X - 1 },
        Point#point{ y = Y - 1 },
        Point#point{ x = X + 1 }
    ].



-spec checkIfSubgroup(points(), points()) -> boolean().
checkIfSubgroup([], _) -> true;
checkIfSubgroup([Point | Points], Group) ->
    case lists:member(Point, Group) of
        true -> checkIfSubgroup(Points, Group);
        false -> false
    end.

% inBoardAroundPoints(Point, Board) ->
%     [P || P <- aroundPoints(Point), pointValue(P, Board) =/= ?OUT_OF_BOARD].



%%%
%%% Working with Matrix Board
%%%

-spec pointValue(#point{}, board()) -> integer().
pointValue(#point{x = X}, Board) when length(Board) < X ->      ?OUT_OF_BOARD;
pointValue(#point{x = X}, _) when X < 1 ->                      ?OUT_OF_BOARD;
pointValue(#point{y = Y}, [Col | _]) when length(Col) < Y ->    ?OUT_OF_BOARD;
pointValue(#point{y = Y}, _) when Y < 1 ->                      ?OUT_OF_BOARD;
pointValue(#point{x = X, y = Y}, Board) ->
    lists:nth(Y, lists:nth(X, Board)).

-spec isFirstLine(#point{}, board()) -> boolean().
isFirstLine(#point{x = X, y = Y}, Board) ->
    X == 1 orelse Y == 1 orelse
    pointValue(#point{x = X + 1}) == ?OUT_OF_BOARD orelse
    pointValue(#point{y = Y + 1}) == ?OUT_OF_BOARD.


%%%
%%% UNIT TESTS
%%%


dead_stones_test() ->
    Board = [
        [0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0],
        [0, 0, 1, 2, 0],
        [0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0]
    ],
    [] = getDeadStones(#point{x = 3, y = 3}, Board),

    Board1 = [
        [0, 0, 0, 0, 0],
        [0, 0, 2, 0, 0],
        [0, 2, 1, 2, 0],
        [0, 0, 2, 0, 0],
        [0, 0, 0, 0, 0]
    ],
    [#point{x = 3, y = 3}] = getDeadStones(#point{x = 3, y = 3}, Board1),

    Board3 = [
        [0, 0, 0, 0, 0],
        [2, 2, 2, 0, 0],
        [0, 1, 1, 2, 0],
        [2, 2, 2, 0, 0],
        [0, 0, 0, 0, 0]
    ],
    [] = getDeadStones(#point{x = 3, y = 3}, Board3),

    Board4 = [
        [0, 0, 0, 0, 0],
        [0, 2, 2, 0, 0],
        [2, 1, 1, 2, 0],
        [1, 2, 2, 0, 0],
        [0, 0, 0, 0, 0]
    ],
    [#point{x = 3, y = 2}, #point{x = 3, y = 3}] = getDeadStones(#point{x = 3, y = 3}, Board4),

    Board5 = [
        [0, 0, 0, 0, 0],
        [0, 0, 0, 0, 1],
        [0, 0, 1, 0, 0],
        [0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0]
    ],
    [] = getDeadStones(#point{x = 3, y = 3}, Board5).

