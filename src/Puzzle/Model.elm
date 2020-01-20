module Puzzle.Model exposing (..)

import Dict exposing (Dict)
import List.Extra
import Maybe exposing (Maybe)


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
      Island Int ConnectionCounts ConnectionCounts


{-| top, right, bottom left
-}
type alias ConnectionCounts =
    { top : Int, right : Int, bottom : Int, left : Int }


connectionCount : Int -> Int -> Int -> Int -> ConnectionCounts
connectionCount top right bottom left =
    { top = top, right = right, bottom = bottom, left = left }


type alias Puzzle =
    { islands : Islands
    , connections : Connections
    , width : Int
    , height : Int
    }


type alias Islands =
    { list : List Island

    -- redundant, optimization for a quick check on island neighbour lookup
    , fields : Dict Int Bool
    }


type alias Connections =
    { list : List Connection
    , -- indices of islands which are connected; redundant, optimization for a quick check on drawn bridges over the x,y positions
      fields : Dict Int ( Int, Int )
    }


{-| smallerIslandIdx, biggerIslandIdx, connectionCount
-}
type alias Connection =
    ( Int, Int, Int )


addIsland : Int -> Int -> Int -> Int -> Int -> Islands -> Islands
addIsland index t r b l islands =
    { list = Island index (connectionCount t r b l) (connectionCount 0 0 0 0) :: islands.list
    , fields = Dict.insert index True islands.fields
    }


addIslandsConnection : Int -> Int -> Puzzle -> Puzzle
addIslandsConnection idx1 idx2 puzzle =
    -- TODO update puzzle.islands and puzzle.connections
    puzzle


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


getIslandFreeConnectionCount : Puzzle -> Island -> Int
getIslandFreeConnectionCount puzzle island =
    -- TODO check what an island is connected to
    0


isIslandFilled : Puzzle -> Island -> Bool
isIslandFilled puzzle island =
    -- TODO check bridges
    False


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


distanceBetweenIslands : Int -> Int -> Int -> Int
distanceBetweenIslands width idx1 idx2 =
    let
        dx =
            idx_x width idx1 - idx_x width idx2

        dy =
            idx_y width idx1 - idx_y width idx2
    in
    max dx dy


directionToConnectionCount : Direction -> ConnectionCounts -> Int
directionToConnectionCount dir { top, right, bottom, left } =
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
