module Type.LocalExtra exposing (references)

import Elm.Syntax.ModuleName
import Elm.Syntax.Node
import Elm.Syntax.Range
import Elm.Syntax.TypeAnnotation


references :
    Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation
    ->
        List
            { lookupRange : Elm.Syntax.Range.Range
            , range : Elm.Syntax.Range.Range
            , moduleName : Elm.Syntax.ModuleName.ModuleName
            , name : String
            }
references (Elm.Syntax.Node.Node _ type_) =
    case type_ of
        Elm.Syntax.TypeAnnotation.GenericType _ ->
            []

        Elm.Syntax.TypeAnnotation.Unit ->
            []

        Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation input output ->
            (input |> references) ++ (output |> references)

        Elm.Syntax.TypeAnnotation.Tupled parts ->
            parts |> List.concatMap references

        Elm.Syntax.TypeAnnotation.Record fields ->
            fields |> List.concatMap (\(Elm.Syntax.Node.Node _ ( _, fieldValue )) -> fieldValue |> references)

        Elm.Syntax.TypeAnnotation.GenericRecord _ (Elm.Syntax.Node.Node _ fields) ->
            fields |> List.concatMap (\(Elm.Syntax.Node.Node _ ( _, fieldValue )) -> fieldValue |> references)

        Elm.Syntax.TypeAnnotation.Typed (Elm.Syntax.Node.Node referenceRange ( moduleName, unqualifiedName )) arguments ->
            arguments
                |> List.concatMap references
                |> (::)
                    { lookupRange = referenceRange
                    , range = referenceRange
                    , moduleName = moduleName
                    , name = unqualifiedName
                    }
