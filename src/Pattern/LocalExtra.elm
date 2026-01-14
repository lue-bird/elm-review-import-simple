module Pattern.LocalExtra exposing (references)

import Elm.Syntax.ModuleName
import Elm.Syntax.Node
import Elm.Syntax.Pattern
import Elm.Syntax.Range


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
                :: (arguments |> List.concatMap references)


syntaxQualifiedNameRefToString : Elm.Syntax.Pattern.QualifiedNameRef -> String
syntaxQualifiedNameRefToString ref =
    case ref.moduleName of
        [] ->
            ref.name

        qualificationPart0 :: qualificationPart1Up ->
            ((qualificationPart0 :: qualificationPart1Up) |> String.join ".")
                ++ "."
                ++ ref.name
