-module(score_estimator).
-include_lib("eunit/include/eunit.hrl").

%% API
-export([estimateScore/1]).
-export([findAllGroups/1]).
-export([random_board/2]).
-export([all_points/1]).
-export([try_all_groups/0]).
-export([try_zones/0]).
-export([try_predefined/0]).

-include("calculations.hrl").
-include("drago.hrl").

%%%
%%% Score Estimator
%%%

estimateScore(_Board) ->
    ok.

%%%
%%% Zones
%%%

-spec findAllZones(board()) -> list(#zone{}).
findAllZones(Board) ->
    convertZones(Board, findAllZones(Board, all_points(Board), [])).

findAllZones(_, [], ZonesFound) -> ZonesFound;
findAllZones(Board, [Point | Points], ZonesFound) ->
    case board_calculations:pointValue(Point, Board) of
        ?EMPTY ->
            case lists:any(fun(Zone) -> lists:member(Point, Zone) end, ZonesFound) of
                false -> findAllZones(Board, Points, [findGroup(Board, Point) | ZonesFound]);
                _true -> findAllZones(Board, Points, ZonesFound)
            end;
        _ -> findAllZones(Board, Points, ZonesFound)
    end.


-spec convertZones(board(), list(list())) -> list(#zone{}).
convertZones(Board, Zones) ->
    [#zone{stones = Zone, contactWith = get_contacts(Board, Zone)} || Zone <- Zones].

%%convertZones(_, [], ConvertedZones) -> ConvertedZones;
%%convertZones(Board, [Zone | Zones], ConvertedZones) ->
%%    #zone{stones = zone}.

%%%
%%% Groups of stones
%%%

-spec findAllGroups(board()) -> list(#group{}).
findAllGroups(Board) ->
    Zones = findAllZones(Board),
    Groups = findAllGroups(Board, all_points(Board), []),

    lists:map(
        fun([S|_] = Stones) ->
            StoneType = board_calculations:pointValue(S, Board),

            %% TODO: filter zones, which truly contact with @Stones
%%            [Z || Z <- Zones, lists:any(fun(P) -> lists:member(P, board_calculations:aroundPoints(P)) end, Stones)],


            IsAlive = case [Z || Z <- Zones, Z#zone.contactWith =:= [StoneType], ] of
                [Zone] when length(Zone#zone.stones) > 5 -> true;
                [_ | _] -> true;
                _ -> undefined
            end,

            #group{stones = Stones, stone_type = StoneType, isAlive = IsAlive}
        end
    , Groups).

findAllGroups(_, [], GroupsFound) -> GroupsFound;
findAllGroups(Board, [Point | Points], GroupsFound) ->
    case board_calculations:pointValue(Point, Board) =/= ?EMPTY of
        true ->
            case lists:any(fun(Group) -> lists:member(Point, Group) end, GroupsFound) of
                false ->
                    findAllGroups(Board, Points, [findGroup(Board, Point) | GroupsFound]);
                _true -> findAllGroups(Board, Points, GroupsFound)
            end;
        _false -> findAllGroups(Board, Points, GroupsFound)
    end.


findGroup(Board, Point) -> findGroup(Board, board_calculations:pointValue(Point, Board), [Point], []).
findGroup(_, _, [], Found) -> Found;
findGroup(Board, StoneType, [PointToConsider | FreshPoints], Found) ->
    case lists:member(PointToConsider, Found) of
        true -> findGroup(Board, StoneType, FreshPoints, Found);
        _false ->
            NewPoints = lists:filter(
                fun(Point) ->
                    not lists:member(Point, FreshPoints) andalso board_calculations:pointValue(Point, Board) =:= StoneType
                end
                , board_calculations:aroundPoints(PointToConsider)),
            findGroup(Board, StoneType, NewPoints ++ FreshPoints, [PointToConsider | Found])
    end.


-spec findSolidGroups(list(list()), list(#group{})) -> list(#group{}).
findSolidGroups(_Board, _AllGroups) ->
    [].



%%%
%%% Utils
%%%


%% Returns board field types who contacts group of Points
%% Group is supposed to be of one type

get_contacts(Board, Points) ->
    [FirstPoint | _] = Points,
    StoneType = board_calculations:pointValue(FirstPoint, Board),
    get_contacts(Board, Points, StoneType, []).
get_contacts(_, [], _, Contacts) -> Contacts;
get_contacts(Board, [Point | Points], StoneType, Contacts) ->
    %% get stone types around @Point , but not StoneType
    LocalContacts = [
        Neighbour || Neighbour <-
        [ board_calculations:pointValue(MatePoint, Board) || MatePoint <- board_calculations:aroundPoints(Point) ],
        Neighbour =/= StoneType, Neighbour =/= ?OUT_OF_BOARD
    ],

    NewContacts = lists:umerge(lists:usort(LocalContacts), Contacts),
    get_contacts(Board, Points, StoneType, NewContacts).


all_points(Board) -> all_points(Board, 1, []).
all_points([], _, Points) -> Points;
all_points([[]], _, Points) -> Points;
all_points([Row | Rows], RowNumber, Points) ->
    Xs  = lists:seq(1, length(Row)),
    NewPoints = Points ++ [#point{y = RowNumber, x = X} || X <- Xs],
    all_points(Rows, RowNumber+1, NewPoints).



print_board([]) -> ok;
print_board([Row | Rows]) ->
    io:format("~p~n", [Row]),
    print_board(Rows).


%% Testing

try_zones() ->
    try_zones(random_board(5, 5)).
try_zones(B) ->
    print_board(B),
    io:format("~n"),
    io:format("points : ~p~n", [all_points(B)]),
    io:format("~n"),
    io:format("all values : ~p~n", [ [board_calculations:pointValue(Point, B) || Point <- all_points(B)] ]),
    io:format("~n"),
    io:format("~p~n", [findAllZones(B)]).

try_predefined() ->
    Board = [
        [1, 1, 2, 0, 0],
        [0, 1, 0, 0, 0],
        [1, 1, 1, 1, 1],
        [0, 1, 1, 2, 0],
        [0, 1, 2, 2, 1]
    ],
    try_zones(Board),
    try_all_groups(Board).

try_all_groups() ->
    try_all_groups(random_board(5, 5)).
try_all_groups(B) ->
    print_board(B),
    findAllGroups(B).


random_board(Rows, Cols) when Rows < 1 orelse Cols < 1 -> undefined;
random_board(Rows, Cols) ->
    random_board(Rows, Cols, 1, 0, [[]]).
random_board(R, C, R, C, Board) -> Board;
random_board(R, C, Rows, C, [Line | Board]) ->
    random_board(R, C, Rows+1, 0, [[] | [Line | Board]]);
random_board(R, C, Rows, Cols, [Line | Board]) ->
    random_board(R, C, Rows, Cols+1, [[random_stone_type() | Line] | Board]).

random_stone_type() ->
    trunc(rand:uniform() * 100) rem 3.


%%%
%%% UNIT TESTS
%%%


groups_test() ->
    ok.
