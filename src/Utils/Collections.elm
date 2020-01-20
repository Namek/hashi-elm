module Utils.Collections exposing (mapNumbersToValues, maybeToList)

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


maybeToList : Maybe a -> List a
maybeToList maybe =
    maybe |> Maybe.map (\a -> [ a ]) |> Maybe.withDefault []
