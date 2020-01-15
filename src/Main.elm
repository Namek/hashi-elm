module Main exposing (main)

import Browser
import Html exposing (Html, div, text)
import Html.Attributes
import Html.Events.Extra.Pointer as Pointer
import Puzzle.Generation exposing (puzzle1)
import Puzzle.Model exposing (..)
import Svg
import Svg.Attributes exposing (..)
import Utils.Misc exposing (either, emptySvg, isMaybeValue, noCmd)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


type alias Model =
    { puzzle : Puzzle
    , islandDrag : IslandDrag

    -- islands' index from, index to
    , temporaryBridge : Maybe ( Int, Int )
    }


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
      , islandDrag = NoIslandsHovered
      , temporaryBridge = Nothing
      }
    , Cmd.none
    )


type IslandDrag
    = NoIslandsHovered
    | FirstIslandHovered Int
    | FirstIslandPinned Int
    | SecondIslandHovered Int Int


xy_idx : Int -> Int -> Int -> Int
xy_idx width x y =
    y * width + x


idx_x : Int -> Int -> Int
idx_x width index =
    modBy width index


idx_y : Int -> Int -> Int
idx_y width index =
    index // width



-- UPDATE


type Msg
    = NoMsg
    | GotIslandHovered Int
    | GotIslandUnhovered
    | GotDragStarted
    | GotDragShouldStop
    | PinIsland Int
    | CheckBridgeDirection ( Float, Float )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        { puzzle } =
            model

        pass =
            model |> noCmd

        getIsland idx =
            getIslandByIndex puzzle.islands idx
    in
    case msg of
        NoMsg ->
            pass

        GotIslandHovered islandIndex ->
            case model.islandDrag of
                NoIslandsHovered ->
                    ( { model | islandDrag = FirstIslandHovered islandIndex }, Cmd.none )

                FirstIslandHovered idx ->
                    ( { model | islandDrag = FirstIslandHovered islandIndex }, Cmd.none )

                FirstIslandPinned idx ->
                    ( { model | islandDrag = SecondIslandHovered idx islandIndex }, Cmd.none )

                SecondIslandHovered idx1 idx2 ->
                    ( { model | islandDrag = SecondIslandHovered idx1 idx2 }, Cmd.none )

        GotIslandUnhovered ->
            case model.islandDrag of
                NoIslandsHovered ->
                    pass

                FirstIslandHovered int ->
                    ( { model | islandDrag = NoIslandsHovered }, Cmd.none )

                FirstIslandPinned idx ->
                    pass

                SecondIslandHovered idx1 idx2 ->
                    ( { model | islandDrag = FirstIslandPinned idx1 }, Cmd.none )

        GotDragStarted ->
            case model.islandDrag of
                NoIslandsHovered ->
                    pass

                FirstIslandHovered idx ->
                    pass

                FirstIslandPinned idx ->
                    pass

                SecondIslandHovered idx1 idx2 ->
                    pass

        GotDragShouldStop ->
            { model | islandDrag = NoIslandsHovered } |> noCmd

        PinIsland idx1 ->
            case getIsland idx1 of
                Just island ->
                    if not (isIslandFilled puzzle island) then
                        { model | islandDrag = FirstIslandPinned idx1 } |> noCmd

                    else
                        pass

                Nothing ->
                    pass

        CheckBridgeDirection ( x, y ) ->
            let
                maybeFirstIslandIndex =
                    case model.islandDrag of
                        FirstIslandPinned idx ->
                            Just idx

                        SecondIslandHovered idx1 idx2 ->
                            Just idx1

                        _ ->
                            Nothing

                direction =
                    -- TODO determine a direction based on x,y and currently the first island
                    Up
            in
            -- draw bridge from first island to any other second if model allows it:
            -- the neighbour island can't be filled and there has to be a clear way to from the first one.
            maybeFirstIslandIndex
                |> Maybe.andThen
                    (\i1_idx ->
                        case findNeighbourIsland puzzle i1_idx direction of
                            Just i2_idx ->
                                Just ( i1_idx, i2_idx )

                            _ ->
                                Nothing
                    )
                |> Maybe.andThen
                    (\( i1_idx, i2_idx ) ->
                        if isThereClearWay puzzle i1_idx i2_idx then
                            Just <| SecondIslandHovered i1_idx i2_idx

                        else
                            Nothing
                    )
                |> Maybe.map
                    (\islandDrag ->
                        { model | islandDrag = islandDrag } |> noCmd
                    )
                |> Maybe.withDefault
                    pass



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ Html.Attributes.style "width" "100vw"
        , Html.Attributes.style "height" "100vh"
        , Pointer.onLeave (always GotDragShouldStop)
        , Pointer.onUp (always GotDragShouldStop)
        ]
        [ text <| String.fromInt <| model.puzzle.width
        , text "x"
        , text <| String.fromInt <| model.puzzle.height
        , renderPuzzle model
        ]


strNum =
    String.fromInt


renderPuzzle model =
    let
        { puzzle } =
            model
    in
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
        , style "user-select: none"
        ]
        (List.append (renderIslands model) (renderBridges puzzle))


scaleFactor =
    5


fieldSize =
    10


circleRadius =
    fieldSize // 2


isIslandHovered : IslandDrag -> Int -> Bool
isIslandHovered drag expectedIslandIndex =
    case drag of
        NoIslandsHovered ->
            False

        FirstIslandHovered idx ->
            idx == expectedIslandIndex

        FirstIslandPinned idx ->
            idx == expectedIslandIndex

        SecondIslandHovered idx1 idx2 ->
            idx1 == expectedIslandIndex || idx2 == expectedIslandIndex


renderIslands : Model -> List (Html Msg)
renderIslands { puzzle, islandDrag } =
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

                isHovered =
                    isIslandHovered islandDrag index
            in
            Svg.g
                [ Pointer.onOver <| (always <| GotIslandHovered index)
                , Pointer.onLeave <| always GotIslandUnhovered
                , Pointer.onDown <| (always <| PinIsland index)
                , Pointer.onMove <| \evt -> CheckBridgeDirection evt.pointer.offsetPos
                ]
                [ renderCircle number posX posY isHovered
                ]
    in
    puzzle.islands |> List.map renderIsland


renderBridges : Puzzle -> List (Html Msg)
renderBridges puzzle =
    let
        renderBridge : Bridge -> Html Msg
        renderBridge bridge =
            case bridge of
                Bridge 0 _ _ _ _ ->
                    emptySvg

                Bridge count maxCount dir from to ->
                    let
                        -- TODO
                        startX =
                            idx_x puzzle.width from * fieldSize + circleRadius

                        startY =
                            idx_y puzzle.width from * fieldSize + circleRadius

                        endX =
                            idx_x puzzle.width to * fieldSize + circleRadius

                        endY =
                            idx_y puzzle.width to * fieldSize + circleRadius
                    in
                    renderLines count startX startY endX endY
    in
    puzzle.bridges |> List.map renderBridge


renderCircle number posX posY isHovered =
    Svg.g []
        [ Svg.circle
            [ cx <| strNum posX
            , cy <| strNum posY
            , r <| strNum <| circleRadius
            , fill <| either (color 255 255 255) (color 255 0 0) <| isHovered
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
        , Svg.Attributes.style "stroke:rgb(255,0,0);stroke-width:0.5"
        ]
        []


color r g b =
    "rgb(" ++ ([ r, g, b ] |> List.map String.fromInt |> String.join ",") ++ ")"
