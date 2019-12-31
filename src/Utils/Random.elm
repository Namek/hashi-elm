module Utils.Random exposing (generateNumber, generateNumbers, scale)

import PseudoRandom


{-| Scales down a number from the range `[0, MAX_INT]` to the number of between the `min` and `max`.

It's better than just clamping value because it preserves the randomization effect.

-}
scale : Int -> Int -> Int -> Int
scale min max randomizedNumber =
    let
        rangeSmall =
            max - min |> toFloat

        x =
            (randomizedNumber |> toFloat) / (PseudoRandom.m0 |> toFloat)
    in
    min + (rangeSmall * x |> truncate)


{-| Generates a one number in range `[min, max]` based on given `seed`.
-}
generateNumber : Int -> Int -> Int -> Int
generateNumber seed min max =
    case PseudoRandom.integerSequence 1 seed of
        x :: _ ->
            scale min max x

        _ ->
            {- this won't happen -}
            0


generateNumbers : Int -> Int -> Int -> Int -> List Int
generateNumbers seed min max amount =
    PseudoRandom.integerSequence amount seed
        |> List.map (scale min max)
