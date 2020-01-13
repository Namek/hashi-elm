module Utils.Collections exposing (mapNumbersToValues)

import List.Extra


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
