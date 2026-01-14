module Pattern.LocalExtra exposing (nodeVariables, references)

import Elm.Syntax.ModuleName
import Elm.Syntax.Node
import Elm.Syntax.Pattern
import Elm.Syntax.Range
import Set exposing (Set)
import Set.LocalExtra


references :
    Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern
    ->
        List
            { lookupRange : Elm.Syntax.Range.Range
            , range : Elm.Syntax.Range.Range
            , moduleName : Elm.Syntax.ModuleName.ModuleName
            , name : String
            }
references (Elm.Syntax.Node.Node patternRange pattern) =
    -- IGNORE TCO
    case pattern of
        Elm.Syntax.Pattern.AllPattern ->
            []

        Elm.Syntax.Pattern.UnitPattern ->
            []

        Elm.Syntax.Pattern.CharPattern _ ->
            []

        Elm.Syntax.Pattern.StringPattern _ ->
            []

        Elm.Syntax.Pattern.IntPattern _ ->
            []

        Elm.Syntax.Pattern.HexPattern _ ->
            []

        Elm.Syntax.Pattern.FloatPattern _ ->
            []

        Elm.Syntax.Pattern.VarPattern _ ->
            []

        Elm.Syntax.Pattern.RecordPattern _ ->
            []

        Elm.Syntax.Pattern.ParenthesizedPattern inParens ->
            references inParens

        Elm.Syntax.Pattern.AsPattern aliased _ ->
            references aliased

        Elm.Syntax.Pattern.UnConsPattern head tail ->
            (tail |> references) ++ (head |> references)

        Elm.Syntax.Pattern.TuplePattern parts ->
            parts |> List.concatMap references

        Elm.Syntax.Pattern.ListPattern elements ->
            elements |> List.concatMap references

        Elm.Syntax.Pattern.NamedPattern fullyQualified arguments ->
            arguments
                |> List.concatMap references
                |> (::)
                    { lookupRange = patternRange
                    , range =
                        { start = patternRange.start
                        , end =
                            { row = patternRange.start.row
                            , column =
                                patternRange.start.column
                                    + (fullyQualified |> syntaxQualifiedNameRefToString |> String.length)
                            }
                        }
                    , moduleName = fullyQualified.moduleName
                    , name = fullyQualified.name
                    }


syntaxQualifiedNameRefToString : Elm.Syntax.Pattern.QualifiedNameRef -> String
syntaxQualifiedNameRefToString ref =
    case ref.moduleName of
        [] ->
            ref.name

        qualificationPart0 :: qualificationPart1Up ->
            ((qualificationPart0 :: qualificationPart1Up) |> String.join ".")
                ++ "."
                ++ ref.name


{-| Recursively find all bindings in a pattern.
-}
nodeVariables : Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern -> Set String
nodeVariables (Elm.Syntax.Node.Node _ pattern) =
    variables pattern


variables : Elm.Syntax.Pattern.Pattern -> Set String
variables pattern =
    -- IGNORE TCO
    case pattern of
        Elm.Syntax.Pattern.VarPattern name ->
            name |> Set.singleton

        Elm.Syntax.Pattern.AsPattern afterAsPattern (Elm.Syntax.Node.Node _ name) ->
            Set.insert name (afterAsPattern |> nodeVariables)

        Elm.Syntax.Pattern.ParenthesizedPattern inParens ->
            inParens |> nodeVariables

        Elm.Syntax.Pattern.ListPattern patterns ->
            patterns |> Set.LocalExtra.unionFromListMap nodeVariables

        Elm.Syntax.Pattern.TuplePattern patterns ->
            patterns |> Set.LocalExtra.unionFromListMap nodeVariables

        Elm.Syntax.Pattern.RecordPattern patterns ->
            patterns |> Set.LocalExtra.fromListMap (\(Elm.Syntax.Node.Node _ name) -> name)

        Elm.Syntax.Pattern.NamedPattern _ patterns ->
            patterns |> Set.LocalExtra.unionFromListMap nodeVariables

        Elm.Syntax.Pattern.UnConsPattern headPattern tailPattern ->
            Set.union (tailPattern |> nodeVariables) (headPattern |> nodeVariables)

        Elm.Syntax.Pattern.AllPattern ->
            Set.empty

        Elm.Syntax.Pattern.UnitPattern ->
            Set.empty

        Elm.Syntax.Pattern.CharPattern _ ->
            Set.empty

        Elm.Syntax.Pattern.StringPattern _ ->
            Set.empty

        Elm.Syntax.Pattern.IntPattern _ ->
            Set.empty

        Elm.Syntax.Pattern.HexPattern _ ->
            Set.empty

        Elm.Syntax.Pattern.FloatPattern _ ->
            Set.empty
