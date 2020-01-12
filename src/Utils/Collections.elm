module Utils.Collections exposing (..)

import List.Extra


mapNumbersToValues : List a -> Int -> List Int -> List a
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
