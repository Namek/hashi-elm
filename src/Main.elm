module Main exposing (main)

import Array exposing (Array)
import Browser
import Html exposing (Html, div, text)
import List.Extra
import Utils.Random exposing (generateNumbers)


main : Program () Model Msg
main =
    Browser.element
        { init = init, view = view, update = update, subscriptions = always Sub.none }


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
    ( { puzzle = generatePuzzle seed width height
      }
    , Cmd.none
    )


type Direction
    = Vertical
    | Horizontal


type IslandData
    = -- Connection counts: top, right, bottom left
      IslandData Int Int Int Int


type Field
    = Island IslandData
      -- indices of the 2 connected islands
    | BridgePart Direction Int Int


type alias Puzzle =
    { fields : Array Field
    , width : Int
    , height : Int
    }


generatePuzzle : Int -> Int -> Int -> Puzzle
generatePuzzle seed width height =
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
    { width = width
    , height = height
    , fields = Array.fromList [ Island 0 2 1 0 ]
    }


mapNumbersToValues : List a -> Int -> List Int -> List (Maybe a)
mapNumbersToValues values firstNumberForFirstValue numbers =
    numbers
        |> List.map
            (\n ->
                let
                    index =
                        n - firstNumberForFirstValue
                in
                List.Extra.getAt index values
            )



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
        ]
