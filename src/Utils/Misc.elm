module Utils.Misc exposing (either, emptySvg, isMaybeValue, noCmd)

import Svg


either : a -> a -> Bool -> a
either a b cond =
    if cond then
        a

    else
        b


isMaybeValue expectedValue theMaybe =
    case theMaybe of
        Just value ->
            expectedValue == value

        Nothing ->
            False


noCmd model =
    ( model, Cmd.none )


emptySvg =
    -- TODO maybe find something better
    Svg.g [] []
