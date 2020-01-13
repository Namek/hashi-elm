module Main exposing (main)

import Array exposing (Array)
import Browser
import Html exposing (Html, div, text)
import Html.Attributes
import Html.Events.Extra.Pointer as Pointer
import List.Extra
import Maybe.Extra
import Svg
import Svg.Attributes exposing (..)
import Svg.Events
import Utils.Collections exposing (mapNumbersToValues)
import Utils.Misc exposing (either, emptySvg, isMaybeValue, noCmd)
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


puzzle1 : Puzzle
puzzle1 =
    let
        width =
            4

        height =
            5

        isl : Int -> Int -> Int -> Int -> Int -> List Island -> List Island
        isl index t r b l list =
            Island index t r b l :: list

        brg =
            Bridge 0

        bridges =
            [ brg 2 Horizontal 0 3
            , brg 1 Vertical 0 16
            , brg 1 Vertical 3 11
            , brg 1 Horizontal 9 11
            , brg 1 Horizontal 16 19
            ]

        islands : List Island
        islands =
            []
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
    , bridges = [] -- TODO
    }



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
