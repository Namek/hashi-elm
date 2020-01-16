module Puzzle.Generation exposing (..)

import Array exposing (Array)
import Puzzle.Model exposing (..)
import Utils.Collections exposing (mapNumbersToValues)
import Utils.Random exposing (generateNumbers)


type Bridge
    = -- current connection count, max connection count, 2 indices (in the array) of the connected islands
      Bridge Int Orientation Int Int


generatePuzzle_bridges : Int -> Int -> Int -> List Bridge
generatePuzzle_bridges seed width height =
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
            genNums 0 2 maxBridgesTotal
                |> mapNumbersToValues [ Vertical, Horizontal ] 1

        bridgeSizes =
            genNums 1 2 maxBridgesTotal
    in
    -- TODO
    []


generatePuzzle : Int -> Int -> Int -> Puzzle
generatePuzzle seed width height =
    let
        bridges =
            generatePuzzle_bridges seed width height

        islands =
            bridgesToIslands bridges
    in
    { width = width
    , height = height
    , islands = islands
    , connections = { list = [], fields = Array.empty }
    }


bridgesToIslands : List Bridge -> Islands
bridgesToIslands bridges =
    -- TODO
    { list = [], fields = Array.empty }


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


puzzle1 : Puzzle
puzzle1 =
    let
        width =
            4

        height =
            5

        isl : Int -> Int -> Int -> Int -> Int -> Islands -> Islands
        isl =
            addIsland

        brg =
            Bridge

        bridges =
            [ brg 2 Horizontal 0 3
            , brg 1 Vertical 0 16
            , brg 1 Vertical 3 11
            , brg 1 Horizontal 9 11
            , brg 1 Horizontal 16 19
            ]

        -- TODO this should be the final solution, not manually defining an island list
        --islands =
        --    bridgesToIslands bridges
        islands : Islands
        islands =
            { list = [], fields = Array.empty }
                |> isl 0 0 2 1 0
                |> isl 3 0 0 1 2
                |> isl 9 0 1 0 0
                |> isl 11 0 0 2 1
                |> isl 16 2 0 0 0
                |> isl 19 1 0 0 1
    in
    { width = width
    , height = height
    , islands = islands
    , connections = { list = [], fields = Array.empty }
    }
