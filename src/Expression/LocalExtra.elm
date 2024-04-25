module Expression.LocalExtra exposing (referencesWithBranchLocalVariables)

import Elm.Syntax.Expression
import Elm.Syntax.ModuleName
import Elm.Syntax.Node
import Elm.Syntax.Range
import Pattern.LocalExtra
import Set exposing (Set)
import Set.LocalExtra
import Type.LocalExtra


referencesWithBranchLocalVariables :
    Set String
    ->
        (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
         -> List { lookupRange : Elm.Syntax.Range.Range, range : Elm.Syntax.Range.Range, moduleName : Elm.Syntax.ModuleName.ModuleName, name : String }
        )
referencesWithBranchLocalVariables branchLocalVariables =
    -- IGNORE TCO
    \(Elm.Syntax.Node.Node expressionRange expression) ->
        case expression of
            Elm.Syntax.Expression.FunctionOrValue qualification unqualifiedName ->
                if branchLocalVariables |> Set.member unqualifiedName then
                    []

                else
                    [ { lookupRange = expressionRange
                      , range = expressionRange
                      , moduleName = qualification
                      , name = unqualifiedName
                      }
                    ]

            Elm.Syntax.Expression.LambdaExpression lambda ->
                (lambda.args |> List.concatMap Pattern.LocalExtra.references)
                    ++ (lambda.expression
                            |> referencesWithBranchLocalVariables
                                (Set.union branchLocalVariables
                                    (lambda.args
                                        |> Set.LocalExtra.unionFromListMap Pattern.LocalExtra.nodeVariables
                                    )
                                )
                       )

            Elm.Syntax.Expression.CaseExpression caseOf ->
                (caseOf.expression |> referencesWithBranchLocalVariables branchLocalVariables)
                    ++ (caseOf.cases
                            |> List.concatMap
                                (\( patternNode, caseExpressionNode ) ->
                                    (patternNode |> Pattern.LocalExtra.references)
                                        ++ (caseExpressionNode
                                                |> referencesWithBranchLocalVariables
                                                    (Set.union branchLocalVariables
                                                        (patternNode |> Pattern.LocalExtra.nodeVariables)
                                                    )
                                           )
                                )
                       )

            Elm.Syntax.Expression.LetExpression letIn ->
                let
                    variablesForWholeLetIn : Set String
                    variablesForWholeLetIn =
                        Set.union branchLocalVariables
                            (letIn.declarations
                                |> Set.LocalExtra.unionFromListMap
                                    (\(Elm.Syntax.Node.Node _ letDeclaration) ->
                                        case letDeclaration of
                                            Elm.Syntax.Expression.LetFunction letFunction ->
                                                letFunction.declaration
                                                    |> Elm.Syntax.Node.value
                                                    |> .name
                                                    |> Elm.Syntax.Node.value
                                                    |> Set.singleton

                                            Elm.Syntax.Expression.LetDestructuring patternNode _ ->
                                                patternNode |> Pattern.LocalExtra.nodeVariables
                                    )
                            )
                in
                (letIn.expression |> referencesWithBranchLocalVariables variablesForWholeLetIn)
                    ++ (letIn.declarations
                            |> List.concatMap
                                (\(Elm.Syntax.Node.Node _ letDeclaration) ->
                                    letDeclaration |> letDeclarationReferencesWithBranchLocalVariables variablesForWholeLetIn
                                )
                       )

            nonUnqualifiedReferenceOrVariable ->
                nonUnqualifiedReferenceOrVariable
                    |> subs
                    |> List.concatMap (referencesWithBranchLocalVariables branchLocalVariables)


letDeclarationReferencesWithBranchLocalVariables :
    Set String
    ->
        (Elm.Syntax.Expression.LetDeclaration
         -> List { lookupRange : Elm.Syntax.Range.Range, range : Elm.Syntax.Range.Range, moduleName : Elm.Syntax.ModuleName.ModuleName, name : String }
        )
letDeclarationReferencesWithBranchLocalVariables branchLocalVariables =
    \letDeclaration ->
        case letDeclaration of
            Elm.Syntax.Expression.LetDestructuring patternNode destructuredExpressionNode ->
                (patternNode |> Pattern.LocalExtra.references)
                    ++ (destructuredExpressionNode |> referencesWithBranchLocalVariables branchLocalVariables)

            Elm.Syntax.Expression.LetFunction letValueOrFunctionDeclaration ->
                [ case letValueOrFunctionDeclaration.signature of
                    Nothing ->
                        []

                    Just (Elm.Syntax.Node.Node _ signature) ->
                        signature.typeAnnotation
                            |> Type.LocalExtra.references
                , letValueOrFunctionDeclaration.declaration
                    |> Elm.Syntax.Node.value
                    |> .arguments
                    |> List.concatMap Pattern.LocalExtra.references
                , (letValueOrFunctionDeclaration.declaration |> Elm.Syntax.Node.value |> .expression)
                    |> referencesWithBranchLocalVariables
                        (Set.union branchLocalVariables
                            (letValueOrFunctionDeclaration.declaration
                                |> Elm.Syntax.Node.value
                                |> .arguments
                                |> Set.LocalExtra.unionFromListMap
                                    (\patternNode -> patternNode |> Pattern.LocalExtra.nodeVariables)
                            )
                        )
                ]
                    |> List.concat


{-| Get all immediate child expressions of an expression
-}
subs : Elm.Syntax.Expression.Expression -> List (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression)
subs expression =
    case expression of
        Elm.Syntax.Expression.LetExpression letBlock ->
            letBlock.expression
                :: (letBlock.declarations
                        |> List.map
                            (\(Elm.Syntax.Node.Node _ letDeclaration) ->
                                case letDeclaration of
                                    Elm.Syntax.Expression.LetFunction letFunction ->
                                        letFunction.declaration |> Elm.Syntax.Node.value |> .expression

                                    Elm.Syntax.Expression.LetDestructuring _ expression_ ->
                                        expression_
                            )
                   )

        Elm.Syntax.Expression.ListExpr expressions ->
            expressions

        Elm.Syntax.Expression.TupledExpression expressions ->
            expressions

        Elm.Syntax.Expression.RecordExpr fields ->
            fields |> List.map (\(Elm.Syntax.Node.Node _ ( _, value )) -> value)

        Elm.Syntax.Expression.RecordUpdateExpression _ setters ->
            setters |> List.map (\(Elm.Syntax.Node.Node _ ( _, newValue )) -> newValue)

        Elm.Syntax.Expression.RecordAccess recordToAccess _ ->
            [ recordToAccess ]

        Elm.Syntax.Expression.Application applicationElements ->
            applicationElements

        Elm.Syntax.Expression.CaseExpression caseBlock ->
            caseBlock.expression
                :: (caseBlock.cases |> List.map (\( _, caseExpression ) -> caseExpression))

        Elm.Syntax.Expression.OperatorApplication _ _ e1 e2 ->
            [ e1, e2 ]

        Elm.Syntax.Expression.IfBlock condition then_ else_ ->
            [ condition, then_, else_ ]

        Elm.Syntax.Expression.LambdaExpression lambda ->
            [ lambda.expression ]

        Elm.Syntax.Expression.ParenthesizedExpression expressionInParens ->
            [ expressionInParens ]

        Elm.Syntax.Expression.Negation expressionInNegation ->
            [ expressionInNegation ]

        Elm.Syntax.Expression.UnitExpr ->
            []

        Elm.Syntax.Expression.Integer _ ->
            []

        Elm.Syntax.Expression.Hex _ ->
            []

        Elm.Syntax.Expression.Floatable _ ->
            []

        Elm.Syntax.Expression.Literal _ ->
            []

        Elm.Syntax.Expression.CharLiteral _ ->
            []

        Elm.Syntax.Expression.GLSLExpression _ ->
            []

        Elm.Syntax.Expression.RecordAccessFunction _ ->
            []

        Elm.Syntax.Expression.FunctionOrValue _ _ ->
            []

        Elm.Syntax.Expression.Operator _ ->
            []

        Elm.Syntax.Expression.PrefixOperator _ ->
            []
