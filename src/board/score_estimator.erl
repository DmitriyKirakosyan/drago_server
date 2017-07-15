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

%%%
%%% Score Estimator
%%%

estimateScore(Board) ->
    Zones = findAllZones(Board),
    Groups = findAllGroups(Board),
    {BGroups, WGroups} = lists:partition(fun(G) -> G#group.stone_type == ?BLACK end, Groups),

    State = #estimating{board = Board, zones = Zones, bGroups = BGroups, wGroups = WGroups},



    print_state(State).

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
convertZones(_, []) -> [];
convertZones(Board, [Zone | Zones]) ->
    {ContactStones, ContactTypes} = get_contacts(Board, Zone),
    [#zone{points = Zone, contactWithTypes = ContactTypes, contactWith = ContactStones} | convertZones(Board, Zones)].

%%convertZones(_, [], ConvertedZones) -> ConvertedZones;
%%convertZones(Board, [Zone | Zones], ConvertedZones) ->
%%    #zone{stones = zone}.

%%%
%%% Groups of stones
%%%

% doGroupsContact(Group1, Group2) ->


-spec findAllGroups(board()) -> list(#group{}).
findAllGroups(Board) ->
    Groups = findAllGroups(Board, all_points(Board), []),

    [#group{
        stones = G,
        stone_type = board_calculations:pointValue(lists:nth(1, G), Board),
        rect = getGroupRect(G)
    } || G <- Groups].

-spec defineGroupsStatus(list(#group{}), list(#zone{}), board()) -> list(#group{}).
defineGroupsStatus(Groups, Zones, Board) ->
%%    io:format("groups before status iteration : ~n~p~n", [Groups]),
    NewGroups = defineGroupsStatus(Groups, Groups, Zones, Board),
%%    io:format("groups after status iteration : ~n~p~n", [NewGroups]),
    case lists:all(fun(G) -> G#group.isAlive =:= true end, NewGroups) of
        true -> NewGroups;
        false -> defineGroupsStatus(NewGroups, Zones, Board)
    end.


-spec defineGroupsStatus(list(#group{}), list(#group{}), list(#zone{}), board()) -> list(#group{}).
defineGroupsStatus([], _, _, _) -> [];
defineGroupsStatus([Group | Groups], AllGroups, AllZones, Board) ->
    ZonesInside = findZonesInsideGroup(Group, Board, AllZones),
    IsDead = case ZonesInside of
        [Zone] when length(Zone#zone.points) > 5 -> false;
        [_ | _] -> false;
        _ -> true
    end,

    Status = if IsDead =:= false ->
            GroupsInside = findGroupsInsideGroup(Group, Group#group.stone_type, Board, AllGroups, []),
            AreGroupsDead = lists:all(fun(G) -> G#group.isAlive =:= false end, GroupsInside),
            if AreGroupsDead =:= false -> undefined; true -> true end;
        true -> IsDead
    end,

    [Group#group{isAlive = Status} | defineGroupsStatus(Groups, AllGroups, AllZones, Board)].


%%%
%%%  Captures
%%%

-spec defineCaptures(#estimating{}) -> #estimating{}.
defineCaptures(State) ->
    % sort b and w groups ?
    % start from the smallest ?



    % define all groups that can not live and that are surely alive


    % find all zones inside, then check for opponent groups inside


    %% thoughts

    %% can i define groups that each one holds links to all connected groups?
    %% if i can, then i probably can easier find out which group is surrounded in zone, for example.
    %% i can get all connected points and then i go through groups and convert connected points to groups

    ok.

-spec inspectZone(#zone{}, stonetype(), board()) -> {IsSafe, DeadGroups}.
inspectZone(Zone, StoneType, Board) ->

    ok.

%%    lists:map(
%%        fun([S|_] = Stones) ->
%%            StoneType = board_calculations:pointValue(S, Board),
%%
%%            %% define surely alive groups
%%
%%            IsAlive = case [Z || Z <- Zones, Z#zone.contactWithTypes =:= [StoneType],
%%                                                checkIfGroupsIntersect(Z#zone.contactWith, Stones) ] of
%%                [Zone] when length(Zone#zone.stones) > 5 -> true;
%%                [_ | _] -> true;
%%                _ -> undefined
%%            end,
%%
%%            #group{stones = Stones, stone_type = StoneType, isAlive = IsAlive}
%%        end
%%    , Groups).

-spec findAllGroups(board(), points(), list(points())) -> list(points()).
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


-spec findGroup(board(), #point{}) -> points().
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


% -spec findSolidGroups(list(list()), list(#group{})) -> list(#group{}).
% findSolidGroups(_Board, _AllGroups) ->
%     [].

%%%
%%% Holes Inside!
%%%

-spec trimPointsInLine(integer(), integer(), integer(), stonetype(), board()) -> points().
trimPointsInLine(X, Y, Length, StoneType, Board) ->
    Points = lists:map(fun(YItem) -> #point{x = X, y = YItem} end, lists:seq(Y, Y + Length-1)),
    IsNotStoneType = fun(Point) -> board_calculations:pointValue(Point, Board) =/= StoneType end,
    IsFirstLine = fun(Point) -> board_calculations:isFirstLine(Point, Board) end,
    CheckIfDrop = fun(Point) -> IsNotStoneType(Point) andalso not IsFirstLine(Point) end,

    lists:dropwhile(CheckIfDrop, lists:reverse(lists:dropwhile(CheckIfDrop, Points))).


-spec trimPoints(#rect{}, stonetype(), board()) -> points().
trimPoints(#rect{width = 0}, _, _) -> [];
trimPoints(#rect{origin = Origin, width = W, height = H} = Rect, StoneType, Board) ->
    lists:flatten(
        [
            trimPointsInLine(Origin#point.x + W-1, Origin#point.y, H, StoneType, Board) |
            trimPoints(Rect#rect{width = W - 1}, StoneType, Board)
        ]).

-spec findZonesInsideGroup(#group{}, board(), list(#zone{})) -> list(#zone{}).
findZonesInsideGroup(Group, Board, AllZones) ->
    io:format("trim group: ~p~n", [Group]),
    Points = trimPoints(Group#group.rect, Group#group.stone_type, Board),
    io:format("trimmed points : ~p~n", [Points]),
    findZonesInsideGroup(Points, Board, AllZones, []).

findZonesInsideGroup([], _, _, FoundZones) -> FoundZones;
findZonesInsideGroup([Point | Points] = AllPoints, Board, AllZones, FoundZones) ->
    case board_calculations:pointValue(Point, Board) of
        ?EMPTY ->
            Zone = getZoneOfPoint(Point, AllZones),
            NewZones = case lists:member(Zone, FoundZones) of
                false ->
                    case board_calculations:checkIfSubgroup(Zone#zone.points, AllPoints) of
                        true -> [Zone | FoundZones];
                        _ -> FoundZones
                    end;
                _ -> FoundZones
            end,
            findZonesInsideGroup(Points, Board, AllZones, NewZones);
        _ ->
            findZonesInsideGroup(Points, Board, AllZones, FoundZones)
    end.


findGroupsInsideGroup([], _, _, _, FoundGroups) -> FoundGroups;
findGroupsInsideGroup([Point | Points] = AllPoints, StoneType, Board, AllGroups, FoundGroups) ->
    case board_calculations:pointValue(Point, Board) of
        V when V == StoneType orelse V == ?EMPTY orelse V == ?OUT_OF_BOARD ->
            findGroupsInsideGroup(Points, StoneType, Board, AllGroups, FoundGroups);
        _ ->
            Group = getGroupOfPoint(Point, AllGroups),
            NewGroups = case lists:member(Group, FoundGroups) of
                           false ->
                               case board_calculations:checkIfSubgroup(Group#group.stones, AllPoints) of
                                   true -> [Group | FoundGroups];
                                   _ -> FoundGroups
                               end;
                           _ -> FoundGroups
                       end,
            findZonesInsideGroup(Points, AllGroups, NewGroups)
    end.


%%%
%%% Utils
%%%

-spec getZoneOfPoint(#point{}, list(#zone{})) -> #zone{}.
getZoneOfPoint(Point, [Zone | Zones]) ->
    case lists:member(Point, Zone#zone.points) of
        true -> Zone;
        false -> getZoneOfPoint(Point, Zones)
    end.

getGroupOfPoint(Point, [Group | Groups]) ->
    case lists:member(Point, Group#group.stones) of
        true -> Group;
        false -> getZoneOfPoint(Point, Groups)
    end.





-spec checkIfGroupsIntersect(points(), points()) -> boolean().
checkIfGroupsIntersect([], _) -> false;
checkIfGroupsIntersect(_, []) -> false;
checkIfGroupsIntersect([Point | Points], PointsGroup) ->
    case lists:member(Point, PointsGroup) of
        true -> true;
        _false -> checkIfGroupsIntersect(Points, PointsGroup)
    end.


%% Returns board field types who contacts group of Points
%% Group is supposed to be of one type

-spec get_contacts(board(), list(#point{})) -> {list(#point{}), list()}.
get_contacts(Board, Points) ->
    [FirstPoint | _] = Points,
    StoneType = board_calculations:pointValue(FirstPoint, Board),
    get_contacts(Board, Points, StoneType).
get_contacts(Board, Points, StoneType) ->
    AllConnectedStones = lists:usort(lists:flatten(
        [board_calculations:aroundPoints(P) || P <- Points]
    )),

    ConnectedStones = [P || P <- AllConnectedStones, board_calculations:pointValue(P, Board) =/= StoneType,
                                                     board_calculations:pointValue(P, Board) =/= ?OUT_OF_BOARD],

    ConnectedTypes = lists:usort([board_calculations:pointValue(P, Board) || P <- ConnectedStones]),

    {ConnectedStones, ConnectedTypes}.

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

print_groups(Groups) ->
    lists:foreach(fun(G) -> io:format("~p~n", [G]) end, Groups).

-spec print_state(#estimating{}) -> ok.
print_state(State) ->
    print_board(State#estimating.board),
    io:format("---black groups---~n"),
    print_groups(State#estimating.bGroups),
    io:format("---black captures---~n"),
    print_groups(State#estimating.bCapturedGroups),
    io:format("-----~n"),
    io:format("---white groups---~n"),
    print_groups(State#estimating.wGroups),
    io:format("---white captures---~n"),
    print_groups(State#estimating.wCapturedGroups),
    io:format("----~n"),
    io:format("--- zones ---~n"),
    print_groups(State#estimating.zones).


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
    estimateScore(Board).
%%    try_zones(Board),
%%    try_all_groups(Board).

try_all_groups() ->
    try_all_groups(random_board(5, 5)).
try_all_groups(B) ->
    print_board(B),
    Zones = findAllZones(B),
    Groups = findAllGroups(B),
    defineGroupsStatus(Groups, Zones, B).


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
