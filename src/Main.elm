module Main exposing (main)

import Array exposing (Array)
import Browser
import Html exposing (Html, div, text)
import List.Extra
import Svg
import Svg.Attributes exposing (..)
import Utils.Collections exposing (mapNumbersToValues)
import Utils.Random exposing (generateNumbers)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


type alias Model =
    { puzzle : Puzzle }


init : flags -> ( Model, Cmd Msg )
init flags =
    let
        width =
            8

        height =
            10

        seed =
            17
    in
    ( { puzzle = puzzle1 -- generatePuzzle seed width height
      }
    , Cmd.none
    )


type Direction
    = Vertical
    | Horizontal


type Island
    = -- position index in array, connection counts: top, right, bottom left
      Island Int Int Int Int Int


type Bridge
    = -- connection count, 2 indices (in the array) of the connected islands
      Bridge Int Direction Int Int


type alias Puzzle =
    { islands : List Island
    , bridges : List Bridge
    , width : Int
    , height : Int
    }


xy_idx : Int -> Int -> Int -> Int
xy_idx width x y =
    y * width + x


idx_x : Int -> Int -> Int
idx_x width index =
    modBy width index


idx_y : Int -> Int -> Int
idx_y width index =
    index // width


puzzle1 : Puzzle
puzzle1 =
    let
        width =
            4

        height =
            5

        --a : Int -> Int -> Int -> Int -> Int -> Array Island -> Array Island
        --a index t r b l =
        --    Array.set index (Island index t r b l)
        a : Int -> Int -> Int -> Int -> Int -> List Island -> List Island
        a index t r b l list =
            Island index t r b l :: list

        bridges =
            [ Bridge 2 Horizontal 0 3
            , Bridge 1 Vertical 0 16
            , Bridge 1 Vertical 3 11
            , Bridge 1 Horizontal 9 11
            , Bridge 1 Horizontal 16 19
            ]

        islands : List Island
        islands =
            --Array.initialize
            --    |> a 0 0 2 1 0
            --    |> a 3 0 0 1 2
            --    |> a 9 0 1 0 0
            --    |> a 11 0 0 2 1
            --    |> a 16 2 0 0 0
            --    |> a 19 1 0 0 1
            []
                |> a 0 0 2 1 0
                |> a 3 0 0 1 2
                |> a 9 0 1 0 0
                |> a 11 0 0 2 1
                |> a 16 2 0 0 0
                |> a 19 1 0 0 1
    in
    { width = width
    , height = height
    , islands = islands
    , bridges = bridges
    }


bridgesToIslands : List Bridge -> List Island
bridgesToIslands bridges =
    -- TODO
    []


generatePuzzle_bridges : Int -> Int -> Int -> List Bridge
generatePuzzle_bridges seed width height =
    let
        genNums =
            generateNumbers seed

        {- edges of the map can have only one type of `Direction` -}
        maxBridgeLengthHorz =
            width - 2

        maxBridgeLengthVert =
            height - 2

        {- corners can't have bridges. Actually, it's not possible to have this many connections -}
        maxBridgesTotal =
            width * height - 4

        bridgeDirections : List (Maybe Direction)
        bridgeDirections =
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
    , bridges = [] -- TODO
    }



-- UPDATE


type Msg
    = NoMsg


update msg model =
    ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ text <| String.fromInt <| model.puzzle.width
        , text "x"
        , text <| String.fromInt <| model.puzzle.height
        , renderPuzzle model.puzzle
        ]


strNum =
    String.fromInt


renderPuzzle puzzle =
    Svg.svg
        [ width <| strNum (puzzle.width * fieldSize * scaleFactor)
        , height <| strNum (puzzle.height * fieldSize * scaleFactor)
        , viewBox <|
            String.join " " <|
                List.map strNum
                    [ -1
                    , -1
                    , (puzzle.width + 1) * fieldSize + 2
                    , (puzzle.height + 1) * fieldSize + 2
                    ]
        ]
        (List.append (renderIslands puzzle) (renderBridges puzzle))


scaleFactor =
    5


fieldSize =
    10


circleRadius =
    fieldSize // 2


renderIslands : Puzzle -> List (Html Msg)
renderIslands puzzle =
    let
        renderIsland : Island -> Html Msg
        renderIsland island =
            let
                ( number, index ) =
                    case island of
                        Island i t r b l ->
                            ( t + r + b + l, i )

                posX =
                    idx_x puzzle.width index * fieldSize + circleRadius

                posY =
                    idx_y puzzle.width index * fieldSize + circleRadius
            in
            renderCircle number posX posY
    in
    puzzle.islands |> List.map renderIsland


renderBridges : Puzzle -> List (Html Msg)
renderBridges puzzle =
    let
        renderBridge : Bridge -> Html Msg
        renderBridge bridge =
            case bridge of
                Bridge count dir from to ->
                    let
                        -- TODO
                        startX =
                            idx_x puzzle.width from * fieldSize + circleRadius

                        startY =
                            idx_y puzzle.width from * fieldSize + circleRadius

                        endX =
                            idx_x puzzle.width to * fieldSize - circleRadius

                        endY =
                            idx_y puzzle.width to * fieldSize - circleRadius
                    in
                    renderLines count startX startY endX endY
    in
    puzzle.bridges |> List.map renderBridge


renderCircle number posX posY =
    Svg.g []
        [ Svg.circle
            [ cx <| strNum posX
            , cy <| strNum posY
            , r <| strNum <| circleRadius
            , fill <| color 255 255 255
            , stroke <| color 0 0 0
            ]
            []
        , Svg.text_
            [ x <| strNum <| posX
            , y <| strNum <| posY
            , Svg.Attributes.textAnchor "middle"
            , Svg.Attributes.dominantBaseline "central"
            , Svg.Attributes.fontSize <| strNum circleRadius ++ "pt"
            ]
            [ Svg.tspan []
                [ Svg.text <| String.fromInt <| number ]
            ]
        ]


renderLines count startX startY endX endY =
    -- TODO count
    Svg.line
        [ x1 <| strNum startX
        , y1 <| strNum startY
        , x2 <| strNum endX
        , y2 <| strNum endY

        --, Svg.style ""
        ]
        []


color r g b =
    "rgb(" ++ ([ r, g, b ] |> List.map String.fromInt |> String.join ",") ++ ")"
