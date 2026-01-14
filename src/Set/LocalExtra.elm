module Set.LocalExtra exposing (fromListMap, unionFromListMap)

import Set exposing (Set)


unionFromListMap :
    (element -> Set comparableElement)
    -> (List element -> Set comparableElement)
unionFromListMap elementToSet list =
    list
        |> List.foldl
            (\element soFar ->
                Set.union soFar (element |> elementToSet)
            )
            Set.empty


fromListMap : (element -> comparableElement) -> (List element -> Set comparableElement)
fromListMap toComparable list =
    list
        |> List.foldl
            (\element acc ->
                acc |> Set.insert (element |> toComparable)
            )
            Set.empty
