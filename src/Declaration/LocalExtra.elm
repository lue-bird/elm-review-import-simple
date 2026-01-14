module Declaration.LocalExtra exposing (surfaceReferences)

import Elm.Syntax.Declaration
import Elm.Syntax.ModuleName
import Elm.Syntax.Node
import Elm.Syntax.Range
import Pattern.LocalExtra
import Type.LocalExtra


surfaceReferences :
    Elm.Syntax.Declaration.Declaration
    ->
        List
            { lookupRange : Elm.Syntax.Range.Range
            , range : Elm.Syntax.Range.Range
            , moduleName : Elm.Syntax.ModuleName.ModuleName
            , name : String
            }
surfaceReferences declaration =
    case declaration of
        Elm.Syntax.Declaration.FunctionDeclaration functionDeclaration ->
            (case functionDeclaration.signature of
                Nothing ->
                    []

                Just (Elm.Syntax.Node.Node _ signature) ->
                    signature.typeAnnotation
                        |> Type.LocalExtra.references
            )
                ++ (functionDeclaration.declaration
                        |> Elm.Syntax.Node.value
                        |> .arguments
                        |> List.concatMap Pattern.LocalExtra.references
                   )

        Elm.Syntax.Declaration.AliasDeclaration typeAliasDeclaration ->
            typeAliasDeclaration.typeAnnotation |> Type.LocalExtra.references

        Elm.Syntax.Declaration.CustomTypeDeclaration variantType ->
            variantType.constructors
                |> List.concatMap
                    (\(Elm.Syntax.Node.Node _ variant) ->
                        variant.arguments |> List.concatMap Type.LocalExtra.references
                    )

        Elm.Syntax.Declaration.PortDeclaration signature ->
            signature.typeAnnotation |> Type.LocalExtra.references

        -- not supported
        Elm.Syntax.Declaration.InfixDeclaration _ ->
            []

        -- invalid
        Elm.Syntax.Declaration.Destructuring _ _ ->
            []
