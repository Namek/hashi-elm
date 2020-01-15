module Puzzle.Model exposing (..)

import List.Extra


type Orientation
    = Vertical
    | Horizontal


type Direction
    = Up
    | Right
    | Down
    | Left


type Island
    = -- position index in array, connection counts: top, right, bottom left
      Island Int Int Int Int Int


type Bridge
    = -- current connection count, max connection count, 2 indices (in the array) of the connected islands
      Bridge Int Int Orientation Int Int


type alias Puzzle =
    { islands : List Island
    , bridges : List Bridge
    , width : Int
    , height : Int
    }


unwrapIslandIndex : Island -> Int
unwrapIslandIndex island =
    case island of
        Island index _ _ _ _ ->
            index


getIslandByIndex : List Island -> Int -> Maybe Island
getIslandByIndex islands idx =
    List.Extra.find
        (\island ->
            case island of
                Island index _ _ _ _ ->
                    index == idx
        )
        islands


getIslandFreeConnectionCount : Puzzle -> Island -> Int
getIslandFreeConnectionCount puzzle island =
    -- TODO check what an island is connected to
    0


isIslandFilled : Puzzle -> Island -> Bool
isIslandFilled puzzle island =
    -- TODO check bridges
    False


{-| Find a closest island to the given one. Do not check for collisions.
-}
findNeighbourIsland : Puzzle -> Int -> Direction -> Maybe Int
findNeighbourIsland puzzle islandIndex direction =
    getIslandByIndex puzzle.islands islandIndex
        |> Maybe.map
            (\island ->
                case island of
                    Island idx t r b l ->
                        -- TODO find the neighbour of this one
                        idx
            )


findBridgeBetweenIslands : Puzzle -> Int -> Int -> Maybe Bridge
findBridgeBetweenIslands puzzle i1_idx i2_idx =
    puzzle.bridges
        |> List.Extra.find
            (\bridge ->
                isBridgeForIslands i1_idx i2_idx bridge
            )


isBridgeForIslands : Int -> Int -> Bridge -> Bool
isBridgeForIslands i1_idx i2_idx bridge =
    case bridge of
        Bridge count maxCount _ idx1 idx2 ->
            (( idx1, idx2 ) == ( i1_idx, i2_idx ))
                || (( idx2, idx1 ) == ( i1_idx, i2_idx ))


anyCollisionsOtherThan : Puzzle -> Bridge -> Bool
anyCollisionsOtherThan puzzle bridge =
    -- TODO: look at all bridges (except the given one) to find out if any of those crosses the path of this one
    False


{-| Finds if there are available bridge connections to set between two islands. Checks for collisions.
-}
getCurrentMaxConnections : Puzzle -> Int -> Int -> ( Int, Int )
getCurrentMaxConnections puzzle i1_idx i2_idx =
    findBridgeBetweenIslands puzzle i1_idx i2_idx
        |> Maybe.andThen
            (\bridge ->
                if anyCollisionsOtherThan puzzle bridge then
                    Just bridge

                else
                    Nothing
            )
        |> Maybe.map (\(Bridge count maxCount _ _ _) -> ( count, maxCount ))
        |> Maybe.withDefault ( 0, 0 )


isThereClearWay : Puzzle -> Int -> Int -> Bool
isThereClearWay puzzle i1_idx i2_idx =
    getCurrentMaxConnections puzzle i1_idx i2_idx
        |> (\( current, max ) -> max > 0)


{-| Circles between [0, 1, ... max] connections between 2 islands.
-}
switchConnectionCount : Island -> Island -> Bridge -> Bridge
switchConnectionCount i1 i2 bridge =
    -- TODO
    bridge


bridgesToIslands : List Bridge -> List Island
bridgesToIslands bridges =
    -- TODO
    []
