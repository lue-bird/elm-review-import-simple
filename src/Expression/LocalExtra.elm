module Expression.LocalExtra exposing (surfaceReferences)

import Elm.Syntax.Expression
import Elm.Syntax.ModuleName
import Elm.Syntax.Node
import Elm.Syntax.Range
import Pattern.LocalExtra
import Type.LocalExtra


surfaceReferences :
    Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
    ->
        List
            { lookupRange : Elm.Syntax.Range.Range
            , range : Elm.Syntax.Range.Range
            , moduleName : Elm.Syntax.ModuleName.ModuleName
            , name : String
            }
surfaceReferences (Elm.Syntax.Node.Node expressionRange expression) =
    case expression of
        Elm.Syntax.Expression.FunctionOrValue qualification unqualifiedName ->
            [ { lookupRange = expressionRange
              , range = expressionRange
              , moduleName = qualification
              , name = unqualifiedName
              }
            ]

        Elm.Syntax.Expression.RecordUpdateExpression (Elm.Syntax.Node.Node recordVariableRange recordVariable) _ ->
            [ { lookupRange = recordVariableRange
              , range = recordVariableRange
              , moduleName = []
              , name = recordVariable
              }
            ]

        Elm.Syntax.Expression.LambdaExpression lambda ->
            lambda.args |> List.concatMap Pattern.LocalExtra.references

        Elm.Syntax.Expression.CaseExpression caseOf ->
            caseOf.cases
                |> List.concatMap
                    (\( patternNode, _ ) ->
                        patternNode |> Pattern.LocalExtra.references
                    )

        Elm.Syntax.Expression.LetExpression letIn ->
            letIn.declarations
                |> List.concatMap
                    (\(Elm.Syntax.Node.Node _ letDeclaration) ->
                        letDeclaration |> letDeclarationSurfaceReferences
                    )

        _ ->
            []


letDeclarationSurfaceReferences :
    Elm.Syntax.Expression.LetDeclaration
    ->
        List
            { lookupRange : Elm.Syntax.Range.Range
            , range : Elm.Syntax.Range.Range
            , moduleName : Elm.Syntax.ModuleName.ModuleName
            , name : String
            }
letDeclarationSurfaceReferences letDeclaration =
    case letDeclaration of
        Elm.Syntax.Expression.LetDestructuring patternNode _ ->
            patternNode |> Pattern.LocalExtra.references

        Elm.Syntax.Expression.LetFunction letValueOrFunctionDeclaration ->
            (case letValueOrFunctionDeclaration.signature of
                Nothing ->
                    []

                Just (Elm.Syntax.Node.Node _ signature) ->
                    signature.typeAnnotation
                        |> Type.LocalExtra.references
            )
                ++ (letValueOrFunctionDeclaration.declaration
                        |> Elm.Syntax.Node.value
                        |> .arguments
                        |> List.concatMap Pattern.LocalExtra.references
                   )
