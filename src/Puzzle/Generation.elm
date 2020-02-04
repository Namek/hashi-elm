module Puzzle.Generation exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import List.Extra
import Puzzle.Model exposing (..)
import Utils.Collections exposing (mapNumbersToValues)
import Utils.Misc exposing (either)
import Utils.Random exposing (generateNumbers)


type Bridge
    = -- connection count, orientation, 2 indices (in the array) of the connected islands
      Bridge Int Orientation Int Int


generatePuzzle_bridges : Int -> Int -> Int -> Int -> List Bridge
generatePuzzle_bridges seed width height maxConnectionCount =
    let
        genNums =
            generateNumbers seed

        {- edges of the map can have only one type of `Orientation` -}
        maxBridgeLengthHorz =
            width - 2

        maxBridgeLengthVert =
            height - 2

        {- corners can't have bridges. Actually, it's not possible to have this many connections -}
        maxBridgesTotal =
            width * height - 4

        bridgeOrientations : List (Maybe Orientation)
        bridgeOrientations =
            genNums 0 maxConnectionCount maxBridgesTotal
                |> mapNumbersToValues [ Vertical, Horizontal ] 1

        bridgeSizes =
            genNums 1 maxConnectionCount maxBridgesTotal
    in
    -- TODO
    []


generatePuzzle : Int -> Int -> Int -> Puzzle
generatePuzzle seed width height =
    let
        maxConnectionCount =
            2

        bridges =
            generatePuzzle_bridges seed width height maxConnectionCount

        islands =
            bridgesToIslands width bridges
    in
    { width = width
    , height = height
    , islands = islands
    , connections = { list = [], fields = Dict.empty }
    , maxConnectionCount = maxConnectionCount
    }


getBridgesOfIsland : List Bridge -> Int -> List ( Bridge, Int )
getBridgesOfIsland bridges islandIndex =
    bridges
        |> List.filterMap
            (\bridge ->
                case bridge of
                    Bridge _ _ idx1 idx2 ->
                        let
                            secondIdx =
                                if idx1 == islandIndex then
                                    idx2

                                else
                                    idx1
                        in
                        if idx1 == islandIndex || idx2 == islandIndex then
                            Just ( bridge, secondIdx )

                        else
                            Nothing
            )


directionToConnectionSizes : Int -> Direction -> ConnectionSizes
directionToConnectionSizes count dir =
    { top = either count 0 <| dir == Up
    , right = either count 0 <| dir == Right
    , bottom = either count 0 <| dir == Down
    , left = either count 0 <| dir == Left
    }


sumConnectionSizes conn1 conn2 =
    { top = conn1.top + conn2.top
    , right = conn1.right + conn2.right
    , bottom = conn1.bottom + conn2.bottom
    , left = conn1.left + conn2.left
    }


bridgesToIslands : Int -> List Bridge -> Islands
bridgesToIslands mapWidth bridges =
    let
        increaseIsland : Int -> ConnectionSizes -> Islands -> Islands
        increaseIsland index connectionSizesToAdd islands =
            case Dict.get index islands.fields of
                Just _ ->
                    { islands
                        | list =
                            islands.list
                                |> List.Extra.updateIf
                                    (\(Island idx _ _) -> idx == index)
                                    (\(Island idx maxConns curConns) ->
                                        Island idx (sumConnectionSizes maxConns connectionSizesToAdd) curConns
                                    )
                    }

                Nothing ->
                    { list = Island index connectionSizesToAdd (connectionSize 0 0 0 0) :: islands.list
                    , fields = Dict.insert index True islands.fields
                    }

        addNextBridge leftBridges state =
            case leftBridges of
                [] ->
                    state

                (Bridge connectionCount orientation idx1 idx2) :: restBridges ->
                    let
                        dir1 =
                            directionFromIsland mapWidth idx1 idx2

                        dir2 =
                            dir1 |> oppositeDirection

                        sizes1 =
                            dir1 |> directionToConnectionSizes connectionCount

                        sizes2 =
                            dir2 |> directionToConnectionSizes connectionCount
                    in
                    addNextBridge restBridges (increaseIsland idx1 sizes1 state |> increaseIsland idx2 sizes2)
    in
    addNextBridge bridges { list = [], fields = Dict.empty }


puzzle1 : Puzzle
puzzle1 =
    let
        width =
            4

        height =
            5

        brg =
            Bridge

        bridges =
            [ brg 2 Horizontal 0 3
            , brg 1 Vertical 0 16
            , brg 1 Vertical 3 11
            , brg 1 Horizontal 9 11
            , brg 1 Horizontal 11 19
            , brg 1 Horizontal 16 19
            ]

        islands =
            bridgesToIslands width bridges
    in
    { width = width
    , height = height
    , islands = islands
    , connections = { list = [], fields = Dict.empty }
    , maxConnectionCount = 2
    }
