module Declaration.LocalExtra exposing (listReferences)

import Elm.Syntax.Declaration
import Elm.Syntax.ModuleName
import Elm.Syntax.Node
import Elm.Syntax.Pattern
import Elm.Syntax.Range
import Expression.LocalExtra
import Pattern.LocalExtra
import Set exposing (Set)
import Set.LocalExtra
import Type.LocalExtra


{-| Declared name (+ possible variant names)
-}
names : Elm.Syntax.Declaration.Declaration -> Set String
names declaration =
    case declaration of
        Elm.Syntax.Declaration.FunctionDeclaration functionDeclaration ->
            functionDeclaration.declaration
                |> Elm.Syntax.Node.value
                |> .name
                |> Elm.Syntax.Node.value
                |> Set.singleton

        Elm.Syntax.Declaration.AliasDeclaration typeAliasDeclaration ->
            typeAliasDeclaration.name |> Elm.Syntax.Node.value |> Set.singleton

        Elm.Syntax.Declaration.CustomTypeDeclaration variantType ->
            variantType.constructors
                |> List.map (\(Elm.Syntax.Node.Node _ variant) -> variant.name |> Elm.Syntax.Node.value)
                |> Set.fromList
                |> Set.insert (variantType.name |> Elm.Syntax.Node.value)

        Elm.Syntax.Declaration.PortDeclaration signature ->
            signature.name |> Elm.Syntax.Node.value |> Set.singleton

        Elm.Syntax.Declaration.InfixDeclaration infixDeclaration ->
            infixDeclaration.operator |> Elm.Syntax.Node.value |> Set.singleton

        -- invalid
        Elm.Syntax.Declaration.Destructuring _ _ ->
            Set.empty


listReferences :
    List Elm.Syntax.Declaration.Declaration
    ->
        List
            { lookupRange : Elm.Syntax.Range.Range
            , range : Elm.Syntax.Range.Range
            , moduleName : Elm.Syntax.ModuleName.ModuleName
            , name : String
            }
listReferences declarations =
    let
        declarationListNames : Set String
        declarationListNames =
            declarations |> Set.LocalExtra.unionFromListMap names
    in
    declarations |> List.concatMap (referencesWithBranchLocalVariables declarationListNames)


referencesWithBranchLocalVariables :
    Set String
    ->
        (Elm.Syntax.Declaration.Declaration
         ->
            List
                { lookupRange : Elm.Syntax.Range.Range
                , range : Elm.Syntax.Range.Range
                , moduleName : Elm.Syntax.ModuleName.ModuleName
                , name : String
                }
        )
referencesWithBranchLocalVariables branchLocalVariables declaration =
    case declaration of
        Elm.Syntax.Declaration.FunctionDeclaration functionDeclaration ->
            let
                argumentPatterns : List (Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern)
                argumentPatterns =
                    functionDeclaration.declaration
                        |> Elm.Syntax.Node.value
                        |> .arguments
            in
            (case functionDeclaration.signature of
                Nothing ->
                    []

                Just (Elm.Syntax.Node.Node _ signature) ->
                    signature
                        |> .typeAnnotation
                        |> Type.LocalExtra.references
            )
                ++ (functionDeclaration.declaration
                        |> Elm.Syntax.Node.value
                        |> .expression
                        |> Expression.LocalExtra.referencesWithBranchLocalVariables
                            (Set.union branchLocalVariables
                                (argumentPatterns |> Set.LocalExtra.unionFromListMap Pattern.LocalExtra.nodeVariables)
                            )
                   )
                ++ (argumentPatterns
                        |> List.concatMap Pattern.LocalExtra.references
                   )

        Elm.Syntax.Declaration.AliasDeclaration typeAliasDeclaration ->
            typeAliasDeclaration.typeAnnotation |> Type.LocalExtra.references

        Elm.Syntax.Declaration.CustomTypeDeclaration variantType ->
            variantType.constructors
                |> List.concatMap (\(Elm.Syntax.Node.Node _ variant) -> variant.arguments)
                |> List.concatMap Type.LocalExtra.references

        Elm.Syntax.Declaration.PortDeclaration signature ->
            signature.typeAnnotation |> Type.LocalExtra.references

        -- not supported
        Elm.Syntax.Declaration.InfixDeclaration _ ->
            []

        -- invalid
        Elm.Syntax.Declaration.Destructuring _ _ ->
            []
