-module(calc_groups).
-author("dmitriy").

%% API
-export([]).

-include("calculations.hrl").


-spec findAllGroups(board()) -> list(#group{}).
findAllGroups(Board) ->
    Groups = findAllGroups(Board, score_estimator:all_points(Board), []),

    [#group{
        stones = G,
        stone_type = board_calculations:pointValue(lists:nth(1, G), Board),
        rect = getGroupRect(G)
    } || G <- Groups].


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


-spec getGroupRect(points()) -> #rect{}.
getGroupRect([]) -> #rect{};
getGroupRect([Point | Points] = AllPoints) ->
    Origin = lists:foldl(
        fun(Item, MinItem) ->
            #point{x = min(Item#point.x, MinItem#point.x), y = min(Item#point.y, MinItem#point.y)}
        end,
        Point, Points),
    {W, H} = lists:foldl(
        fun(Item, {MW, MH}) ->
            {max(MW, Item#point.x - Origin#point.x), max(MH, Item#point.y - Origin#point.y)}
        end,
        {0, 0}, AllPoints),
    #rect{origin = Origin, width = W+1, height = H+1}.
