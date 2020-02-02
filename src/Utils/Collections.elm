module Utils.Collections exposing (findAndGetRest, mapNumbersToValues, maybeToList)

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


{-| Find an element and return it along with the rest of the input items.
-}
findAndGetRest : (a -> Bool) -> List a -> ( Maybe a, List a )
findAndGetRest predicate list =
    let
        iterate checked unchecked =
            case unchecked of
                [] ->
                    ( Nothing, list )

                first :: rest ->
                    if predicate first then
                        ( Just first, List.append (List.reverse checked) rest )

                    else
                        iterate (first :: checked) rest
    in
    iterate [] list
