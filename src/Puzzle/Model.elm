module Puzzle.Model exposing (..)

import Dict exposing (Dict)
import List.Extra
import Maybe exposing (Maybe)
import Maybe.Extra
import Utils.Collections exposing (findAndGetRest)
import Utils.Misc exposing (either)


type Orientation
    = Vertical
    | Horizontal


type Direction
    = Up
    | Right
    | Down
    | Left


type Island
    = -- position index in array, max connection counts, current connection counts
      Island Int ConnectionSizes ConnectionSizes


{-| top, right, bottom left
-}
type alias ConnectionSizes =
    { top : Int, right : Int, bottom : Int, left : Int }


connectionSize : Int -> Int -> Int -> Int -> ConnectionSizes
connectionSize top right bottom left =
    { top = top, right = right, bottom = bottom, left = left }


type alias Puzzle =
    { islands : Islands
    , connections : Connections
    , width : Int
    , height : Int
    , maxConnectionCount : Int
    }


type alias Islands =
    { list : List Island

    -- redundant, optimization for a quick check on island neighbour lookup
    , fields : Dict Int Bool
    }


type alias Connections =
    { list : List Connection
    , -- indices of islands which are connected; redundant, optimization for a quick check on drawn bridges over the x,y positions
      fields : ConnectionsFields
    }


type alias ConnectionsFields =
    Dict Int ( Int, Int )


{-| smallerIslandIdx, biggerIslandIdx, connectionSize
-}
type alias Connection =
    ( Int, Int, Int )


addIsland : Int -> Int -> Int -> Int -> Int -> Islands -> Islands
addIsland index t r b l islands =
    { list = Island index (connectionSize t r b l) (connectionSize 0 0 0 0) :: islands.list
    , fields = Dict.insert index True islands.fields
    }


changeConnectionSizeForIslandNeighbour : Island -> Direction -> Int -> Island
changeConnectionSizeForIslandNeighbour island neighbourDirection newConnectionSize =
    case island of
        Island idx maxConns { top, right, bottom, left } ->
            let
                newTop =
                    neighbourDirection == Up |> either newConnectionSize top

                newLeft =
                    neighbourDirection == Left |> either newConnectionSize left

                newRight =
                    neighbourDirection == Right |> either newConnectionSize right

                newBottom =
                    neighbourDirection == Down |> either newConnectionSize bottom
            in
            Island idx maxConns { top = newTop, right = newRight, bottom = newBottom, left = newLeft }


switchValue : Int -> Int -> Int -> Int
switchValue min max current =
    let
        step =
            current + 1
    in
    if step > max then
        min

    else
        step


{-| Do the most basic mechanic of the game - switch a connection size between two islands.

Does not check for a collision since it is called after the drag is stopped.
However, it does check the current states of islands.

-}
switchIslandConnections : Int -> Int -> Puzzle -> Puzzle
switchIslandConnections idx1 idx2 puzzle =
    let
        ( sortedIdx1, sortedIdx2 ) =
            sortedIndex idx1 idx2

        island1 =
            getIslandByIndex puzzle.islands sortedIdx1

        island2 =
            getIslandByIndex puzzle.islands sortedIdx2

        ( maybeConn, restConns ) =
            findAndGetRest (connectionFilterPredicate sortedIdx1 sortedIdx2) puzzle.connections.list

        -- we need to check up local maximums for both islands
        commonMaxNewConnectionsCount =
            min
                (island1 |> Maybe.map getIslandFreeConnectionSize |> Maybe.withDefault 0)
                (island2 |> Maybe.map getIslandFreeConnectionSize |> Maybe.withDefault 0)

        ( newConnectionList, newConnectionSize ) =
            case maybeConn of
                Nothing ->
                    -- insert a new connection
                    ( ( sortedIdx1, sortedIdx2, 1 ) :: puzzle.connections.list, 1 )

                Just ( _, _, currentConnCount ) ->
                    let
                        commonMaxConnectionSize =
                            min puzzle.maxConnectionCount (currentConnCount + commonMaxNewConnectionsCount)

                        newConnectionSize_ =
                            currentConnCount |> switchValue 0 commonMaxConnectionSize
                    in
                    -- replace the found one
                    ( ( sortedIdx1, sortedIdx2, newConnectionSize_ ) :: restConns, newConnectionSize_ )

        dir1to2 =
            directionFromIsland puzzle.width sortedIdx1 sortedIdx2

        newConnectionFields : ConnectionsFields
        newConnectionFields =
            let
                ( dx, dy ) =
                    directionToPosDiff dir1to2

                startX =
                    idx_x puzzle.width sortedIdx1

                startY =
                    idx_y puzzle.width sortedIdx1

                stepCount =
                    distanceBetweenIslands puzzle.width sortedIdx1 sortedIdx2 - 2

                iterate x y fields leftSteps =
                    let
                        idx =
                            xy_idx puzzle.width x y
                    in
                    case ( Dict.get idx fields, newConnectionSize ) of
                        ( Just _, 0 ) ->
                            Dict.remove idx fields

                        ( Nothing, _ ) ->
                            Dict.insert idx ( sortedIdx1, sortedIdx2 ) fields

                        _ ->
                            if leftSteps > 0 then
                                iterate (x + dx) (y + dy) fields (leftSteps - 1)

                            else
                                fields
            in
            iterate (startX + dx) (startY + dy) puzzle.connections.fields stepCount

        dir2to1 =
            oppositeDirection dir1to2

        newIsland1 =
            island1 |> Maybe.map (\i1 -> changeConnectionSizeForIslandNeighbour i1 dir1to2 newConnectionSize)

        newIsland2 =
            island2 |> Maybe.map (\i2 -> changeConnectionSizeForIslandNeighbour i2 dir2to1 newConnectionSize)

        newConnections : Connections
        newConnections =
            { list = newConnectionList, fields = newConnectionFields }

        updateEachIsland : List Island -> List Island -> List Island
        updateEachIsland currentIslands leftIslands =
            case leftIslands of
                [] ->
                    currentIslands

                island :: rest ->
                    let
                        islandIdx =
                            unwrapIslandIndex island
                    in
                    List.Extra.updateIf (\(Island idx _ _) -> idx == islandIdx) (always island) currentIslands

        islandsToUpdate : List Island
        islandsToUpdate =
            Maybe.Extra.values [ newIsland1, newIsland2 ]

        newIslandsList =
            updateEachIsland puzzle.islands.list islandsToUpdate

        newIslands : Islands
        newIslands =
            { list = newIslandsList, fields = puzzle.islands.fields }

        newPuzzle =
            { puzzle | connections = newConnections, islands = newIslands }
    in
    newPuzzle


xy_idx : Int -> Int -> Int -> Int
xy_idx width x y =
    y * width + x


idx_x : Int -> Int -> Int
idx_x width index =
    modBy width index


idx_y : Int -> Int -> Int
idx_y width index =
    index // width


unwrapIslandIndex : Island -> Int
unwrapIslandIndex (Island index _ _) =
    index


getIslandByIndex : Islands -> Int -> Maybe Island
getIslandByIndex islands idx =
    List.Extra.find
        (\(Island index _ _) -> index == idx)
        islands.list


getConnection : Connections -> Int -> Int -> Maybe Connection
getConnection conns idx1 idx2 =
    conns.list |> List.Extra.find (connectionFilterPredicate idx1 idx2)


connectionFilterPredicate : Int -> Int -> (Connection -> Bool)
connectionFilterPredicate idx1 idx2 =
    let
        ( sortedIdx1, sortedIdx2 ) =
            sortedIndex idx1 idx2
    in
    \( cidx1, cidx2, _ ) -> ( cidx1, cidx2 ) == ( sortedIdx1, sortedIdx2 )


getIslandFreeConnectionSize : Island -> Int
getIslandFreeConnectionSize island =
    case island of
        Island _ max cur ->
            max.top - cur.top + max.right - cur.right + max.bottom - cur.bottom + max.left - cur.left


isIslandFilled : Island -> Bool
isIslandFilled island =
    getIslandFreeConnectionSize island == 0


directionFromIsland : Int -> Int -> Int -> Direction
directionFromIsland width fromIndex toIndex =
    let
        dx =
            idx_x width toIndex - idx_x width fromIndex

        dy =
            idx_y width toIndex - idx_y width fromIndex
    in
    if dx > 0 then
        Right

    else if dx < 0 then
        Left

    else if dy > 0 then
        Down

    else
        Up


oppositeDirection : Direction -> Direction
oppositeDirection dir =
    case dir of
        Up ->
            Down

        Right ->
            Left

        Down ->
            Up

        Left ->
            Right


distanceBetweenIslands : Int -> Int -> Int -> Int
distanceBetweenIslands width idx1 idx2 =
    let
        dx =
            idx_x width idx1 - idx_x width idx2

        dy =
            idx_y width idx1 - idx_y width idx2
    in
    max dx dy


directionToConnectionSize : Direction -> ConnectionSizes -> Int
directionToConnectionSize dir { top, right, bottom, left } =
    case dir of
        Up ->
            top

        Right ->
            right

        Down ->
            bottom

        Left ->
            left


directionToPosDiff : Direction -> ( Int, Int )
directionToPosDiff dir =
    case dir of
        Up ->
            ( 0, -1 )

        Right ->
            ( 1, 0 )

        Down ->
            ( 0, 1 )

        Left ->
            ( -1, 0 )


traverseToNextIsland : Puzzle -> Int -> Direction -> Maybe Int
traverseToNextIsland puzzle fromIndex direction =
    let
        ( dx, dy ) =
            directionToPosDiff direction

        startX =
            idx_x puzzle.width fromIndex

        startY =
            idx_y puzzle.width fromIndex

        iterate x y =
            if x < 0 || x >= puzzle.width || y >= puzzle.height || y < 0 then
                Nothing

            else
                let
                    idx =
                        xy_idx puzzle.width x y
                in
                case Dict.get idx puzzle.islands.fields of
                    Just True ->
                        Just idx

                    _ ->
                        iterate (x + dx) (y + dy)
    in
    --  iterate until another Island is found OR we get out of map
    iterate (startX + dx) (startY + dy)


{-| Find a closest island to the given one, in given direction. Do not check for collisions.
-}
findNeighbourIsland : Puzzle -> Int -> Direction -> Maybe Int
findNeighbourIsland puzzle islandIndex direction =
    getIslandByIndex puzzle.islands islandIndex
        |> Maybe.andThen
            (\(Island _ _ _) ->
                traverseToNextIsland puzzle islandIndex direction
            )


sortedIndex : Int -> Int -> ( Int, Int )
sortedIndex idx1 idx2 =
    ( min idx1 idx2, max idx1 idx2 )


{-| Check if there is no collision in a way between two puzzles.
-}
isThereClearWay : Puzzle -> Int -> Int -> Bool
isThereClearWay puzzle fromIndex toIndex =
    let
        direction =
            directionFromIsland puzzle.width fromIndex toIndex

        ( dx, dy ) =
            directionToPosDiff direction

        ( startX, startY ) =
            ( idx_x puzzle.width fromIndex
            , idx_y puzzle.width fromIndex
            )

        ( endX, endY ) =
            ( idx_x puzzle.width toIndex
            , idx_y puzzle.width toIndex
            )

        expectedTarget =
            sortedIndex fromIndex toIndex

        iterate x y =
            if x < 0 || x >= puzzle.width || y >= puzzle.height || y < 0 then
                False

            else if x == endX && y == endY then
                True

            else
                let
                    idx =
                        xy_idx puzzle.width x y
                in
                case Dict.get idx puzzle.connections.fields of
                    Just ( idx1, idx2 ) ->
                        -- If there is connection but it's the one between
                        -- those same islands then we don't break the traversal.
                        if ( idx1, idx2 ) == expectedTarget then
                            iterate (x + dx) (y + dy)

                        else
                            False

                    _ ->
                        iterate (x + dx) (y + dy)
    in
    iterate (startX + dx) (startY + dy)
