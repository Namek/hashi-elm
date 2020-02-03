module Main exposing (main)

import Browser
import Html exposing (Html, div, text)
import Html.Attributes
import Html.Events
import Html.Events.Extra.Pointer as Pointer
import List.Extra
import Maybe.Extra
import Puzzle.Generation exposing (puzzle1)
import Puzzle.Model exposing (..)
import Svg
import Svg.Attributes exposing (..)
import Utils.Collections exposing (maybeToList)
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
    , puzzle_start : Puzzle
    , isPuzzleDone : Bool
    , islandDrag : IslandDrag
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

        puzzle =
            puzzle1

        -- generatePuzzle seed width height
    in
    ( { puzzle = puzzle
      , puzzle_start = puzzle
      , isPuzzleDone = False
      , islandDrag = NoIslandsHovered
      }
    , Cmd.none
    )


type IslandDrag
    = NoIslandsHovered
    | FirstIslandHovered Int
    | FirstIslandPinned Int
      -- percent, island1Index, island2Index
    | SecondIslandPicked Float Int Int



-- UPDATE


type Msg
    = NoMsg
    | GotIslandHovered Int
    | GotIslandUnhovered
    | GotDragStarted
    | GotDragShouldStop
    | PinIsland Int
    | CheckBridgeDirection ( Float, Float )
    | Reset


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

                FirstIslandPinned _ ->
                    pass

                SecondIslandPicked _ _ _ ->
                    pass

        GotIslandUnhovered ->
            case model.islandDrag of
                NoIslandsHovered ->
                    pass

                FirstIslandHovered int ->
                    ( { model | islandDrag = NoIslandsHovered }, Cmd.none )

                FirstIslandPinned idx ->
                    pass

                SecondIslandPicked _ idx1 idx2 ->
                    pass

        GotDragStarted ->
            case model.islandDrag of
                NoIslandsHovered ->
                    pass

                FirstIslandHovered idx ->
                    pass

                FirstIslandPinned idx ->
                    pass

                SecondIslandPicked _ idx1 idx2 ->
                    pass

        GotDragShouldStop ->
            case model.islandDrag of
                SecondIslandPicked percent idx1 idx2 ->
                    let
                        newPuzzle =
                            switchIslandConnections idx1 idx2 puzzle

                        isPuzzleDone =
                            isSuccessfullyFinished newPuzzle
                    in
                    { model | islandDrag = NoIslandsHovered, puzzle = newPuzzle, isPuzzleDone = isPuzzleDone } |> noCmd

                _ ->
                    { model | islandDrag = NoIslandsHovered } |> noCmd

        PinIsland idx1 ->
            { model | islandDrag = FirstIslandPinned idx1 } |> noCmd

        CheckBridgeDirection ( x, y ) ->
            let
                maybeFirstIslandIndex =
                    case model.islandDrag of
                        FirstIslandPinned idx ->
                            Just idx

                        SecondIslandPicked _ idx1 idx2 ->
                            Just idx1

                        _ ->
                            Nothing
            in
            -- draw bridge from first island to any other second if model allows it:
            -- the neighbour island can't be filled and there has to be a clear way to from the first one.
            maybeFirstIslandIndex
                |> Maybe.andThen
                    (\i1_idx ->
                        let
                            ( islandX, islandY ) =
                                getIslandRenderPos puzzle.width i1_idx

                            ( rescaledX, rescaledY ) =
                                rescalePos_InputToLogic puzzle ( x, y )

                            direction =
                                directionFromPoint
                                    (physicalRenderPos ( rescaledX, rescaledY ))
                                    (physicalRenderPos ( islandX, islandY ))

                            neighbour =
                                findNeighbourIsland puzzle i1_idx direction
                        in
                        case neighbour of
                            Just i2_idx ->
                                let
                                    ( neighbourX, neighbourY ) =
                                        getIslandRenderPos puzzle.width i2_idx

                                    ( to, from, pos ) =
                                        if (abs <| neighbourX - islandX) > (abs <| neighbourY - islandY) then
                                            ( neighbourX, islandX, rescaledX )

                                        else
                                            ( neighbourY, islandY, rescaledY )

                                    distancePercent =
                                        pos |> rescale from to 0.0 1.0 |> Basics.min 1.0
                                in
                                Just ( i1_idx, i2_idx, distancePercent )

                            _ ->
                                Nothing
                    )
                |> Maybe.andThen
                    (\( i1_idx, i2_idx, distancePercent ) ->
                        if canDraw puzzle i1_idx i2_idx then
                            Just <| SecondIslandPicked distancePercent i1_idx i2_idx

                        else
                            case model.islandDrag of
                                SecondIslandPicked _ _ _ ->
                                    -- TODO this does not work??
                                    Just NoIslandsHovered

                                _ ->
                                    Nothing
                    )
                --|> Maybe.Extra.or (Just NoIslandsHovered)
                |> Maybe.map
                    (\islandDrag ->
                        { model | islandDrag = islandDrag } |> noCmd
                    )
                |> Maybe.withDefault
                    pass

        Reset ->
            { model | puzzle = model.puzzle_start } |> noCmd


directionFromPoint ( fromX, fromY ) ( toX, toY ) =
    let
        dx =
            toX - fromX

        dy =
            toY - fromY
    in
    if abs dx > abs dy then
        either Left Right <| (dx > 0)

    else
        either Up Down <| (dy > 0)



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ Html.Attributes.style "width" "100vw"
        , Html.Attributes.style "height" "100vh"
        , Pointer.onLeave (always GotDragShouldStop)
        , Pointer.onUp (always GotDragShouldStop)
        ]
        [ div []
            [ text <| String.fromInt <| model.puzzle.width
            , text "x"
            , text <| String.fromInt <| model.puzzle.height
            , text <| String.fromFloat <| (unwrapTemporaryBridge model.islandDrag |> Maybe.map (\( percent, idx1, idx2 ) -> percent) |> Maybe.withDefault 0.0)
            ]
        , div [] [ Html.button [ Html.Events.onClick Reset ] [ text "Reset" ] ]
        , renderPuzzle model
        , if model.isPuzzleDone then
            div [] [ text "Puzzle Done!" ]

          else
            emptySvg
        ]


strNum =
    String.fromInt


strNumf =
    String.fromFloat


unwrapTemporaryBridge : IslandDrag -> Maybe ( Float, Int, Int )
unwrapTemporaryBridge drag =
    case drag of
        SecondIslandPicked lengthPercent idx1 idx2 ->
            Just ( lengthPercent, idx1, idx2 )

        _ ->
            Nothing


rescale fromLeft fromRight toLeft toRight value =
    let
        fromWidth =
            fromRight - fromLeft

        toWidth =
            toRight - toLeft
    in
    (value - fromLeft) / fromWidth * toWidth + toLeft


renderPuzzle : Model -> Html Msg
renderPuzzle model =
    let
        { puzzle } =
            model

        alreadyDrawnConnections =
            puzzle.connections.list |> List.map (renderConnection puzzle)

        temporaryBridge : Maybe (Html Msg)
        temporaryBridge =
            unwrapTemporaryBridge model.islandDrag
                |> Maybe.map
                    (\( percent, idx1, idx2 ) ->
                        renderTemporaryBridge puzzle idx1 idx2 percent
                    )
    in
    Svg.svg
        [ width <| strNum (puzzle.width * fieldSize * scaleFactor)
        , height <| strNum (puzzle.height * fieldSize * scaleFactor)
        , viewBox <|
            String.join " " <|
                List.map strNum
                    [ -margin
                    , -margin
                    , puzzle.width * fieldSize + margin * 2
                    , puzzle.height * fieldSize + margin * 2
                    ]
        , style "user-select: none"
        , Pointer.onMove <| \evt -> CheckBridgeDirection evt.pointer.offsetPos
        ]
        (List.concat
            [ alreadyDrawnConnections
            , temporaryBridge |> maybeToList
            , renderIslands model

            --, [ renderLine px py (px + 1) py "stroke:rgb(0,127,0);stroke-width:1" ]
            ]
        )


scaleFactor =
    5


margin =
    4


fieldSize =
    10


circleRadius =
    fieldSize // 2


{-| defines the distance between lines
-}
connectionLineMargin =
    3


getIslandRenderPos : Int -> Int -> ( Float, Float )
getIslandRenderPos width index =
    ( idx_x width index * fieldSize + circleRadius |> toFloat
    , idx_y width index * fieldSize + circleRadius |> toFloat
    )


{-| Physical means it's scaled by the scaleFactor and takes margin into the account.
-}
physicalRenderPos : ( Float, Float ) -> ( Float, Float )
physicalRenderPos ( x, y ) =
    ( (x - margin) * scaleFactor, (y - margin) * scaleFactor )


rescalePos_InputToLogic : { a | width : Int, height : Int } -> ( Float, Float ) -> ( Float, Float )
rescalePos_InputToLogic { width, height } ( x, y ) =
    ( rescale 0 (width * fieldSize * scaleFactor |> toFloat) -margin (width * fieldSize + margin |> toFloat) x
    , rescale 0 (height * fieldSize * scaleFactor |> toFloat) -margin (height * fieldSize + margin |> toFloat) y
    )


isIslandHovered : IslandDrag -> Int -> Bool
isIslandHovered drag expectedIslandIndex =
    case drag of
        NoIslandsHovered ->
            False

        FirstIslandHovered idx ->
            idx == expectedIslandIndex

        FirstIslandPinned idx ->
            idx == expectedIslandIndex

        SecondIslandPicked _ idx1 idx2 ->
            idx1 == expectedIslandIndex || idx2 == expectedIslandIndex


renderIslands : Model -> List (Html Msg)
renderIslands { puzzle, islandDrag } =
    let
        renderIsland : Island -> Html Msg
        renderIsland island =
            let
                ( number, index ) =
                    case island of
                        Island idx { top, right, bottom, left } _ ->
                            ( top + right + bottom + left, idx )

                ( posX, posY ) =
                    getIslandRenderPos puzzle.width index

                isHovered =
                    isIslandHovered islandDrag index

                isFilled =
                    isIslandFilled island
            in
            Svg.g
                [ Pointer.onOver <| (always <| GotIslandHovered index)
                , Pointer.onLeave <| always GotIslandUnhovered
                , Pointer.onDown <| (always <| PinIsland index)
                ]
                [ renderCircle number posX posY isHovered isFilled
                ]
    in
    puzzle.islands.list |> List.map renderIsland


renderConnection : Puzzle -> Connection -> Html Msg
renderConnection puzzle conn =
    case conn.connectionSize of
        0 ->
            emptySvg

        count ->
            let
                ( startX, startY ) =
                    getIslandRenderPos puzzle.width conn.idx1

                ( endX, endY ) =
                    getIslandRenderPos puzzle.width conn.idx2
            in
            renderLines count startX startY endX endY conn.orientation


renderLines : Int -> Float -> Float -> Float -> Float -> Orientation -> Html Msg
renderLines count startX startY endX endY orientation =
    let
        m =
            -connectionLineMargin / 2.0

        distances =
            case count of
                1 ->
                    [ 0.0 ]

                2 ->
                    [ -m / 2, m / 2 ]

                3 ->
                    [ -m, 0.0, m ]

                4 ->
                    [ -(m * 3 / 2), -m / 2, m / 2, m * 1.5 ]

                _ ->
                    []

        zeros =
            List.Extra.initialize count (always 0)

        coordDiffs =
            case orientation of
                Vertical ->
                    List.Extra.zip distances zeros

                Horizontal ->
                    List.Extra.zip zeros distances

        lines =
            coordDiffs
                |> List.map
                    (\( dx, dy ) ->
                        renderLine (startX + dx) (startY + dy) (endX + dx) (endY + dy) "stroke:rgb(255,0,0);stroke-width:0.5"
                    )
    in
    Svg.g [] lines


renderTemporaryBridge : Puzzle -> Int -> Int -> Float -> Html Msg
renderTemporaryBridge puzzle from to percent =
    let
        ( startX, startY ) =
            getIslandRenderPos puzzle.width from

        ( toX, toY ) =
            getIslandRenderPos puzzle.width to

        ( dx, dy ) =
            ( toX - startX, toY - startY )

        ( endX, endY ) =
            ( startX + dx * percent, startY + dy * percent )

        lineStyle =
            "stroke:rgb(0,255,0);stroke-width:5"
    in
    renderLine startX startY endX endY lineStyle



-- Non-domain Utils


renderCircle number posX posY isHovered isFilled =
    Svg.g []
        [ Svg.circle
            [ cx <| strNumf posX
            , cy <| strNumf posY
            , r <| strNum <| circleRadius
            , fill <|
                either (color 127 127 127) (either (color 255 255 255) (color 255 0 0) <| isHovered) <|
                    (not isHovered && isFilled)
            , stroke <| color 0 0 0
            ]
            []
        , Svg.text_
            [ x <| strNumf <| posX
            , y <| strNumf <| posY
            , Svg.Attributes.textAnchor "middle"
            , Svg.Attributes.dominantBaseline "central"
            , Svg.Attributes.fontSize <| strNum circleRadius ++ "pt"
            ]
            [ Svg.tspan []
                [ Svg.text <| String.fromInt <| number ]
            ]
        ]


renderLine startX startY endX endY lineStyle =
    Svg.line
        [ x1 <| strNumf startX
        , y1 <| strNumf startY
        , x2 <| strNumf endX
        , y2 <| strNumf endY
        , Svg.Attributes.style lineStyle
        ]
        []


color r g b =
    "rgb(" ++ ([ r, g, b ] |> List.map String.fromInt |> String.join ",") ++ ")"
