module Review.ImportSimple exposing (rule)

{-| Allow only simple imports

@docs rule

-}

import Declaration.LocalExtra
import Elm.Syntax.Exposing
import Elm.Syntax.ModuleName
import Elm.Syntax.Node
import Elm.Syntax.Range
import FastDict
import Review.Fix
import Review.ModuleNameLookupTable
import Review.Rule
import Set exposing (Set)


{-| Report imports that consist of
more than only the module name and exposed matching type name ignoring `.`s

    config =
        [ Review.ImportSimple.rule
        ]


## not reported

    import Audio
    import Audio.Parameter exposing (AudioParameter)


## reported

    import Elm.Syntax.Expression as Expression
    import Parser.Advanced exposing (Parser)
    import ListNonEmpty exposing (ListNonEmpty(..))

[why enable this rule?](https://dark.elm.dmy.fr/packages/lue-bird/elm-review-import-simple/latest#why)

-}
rule : Review.Rule.Rule
rule =
    Review.Rule.newModuleRuleSchemaUsingContextCreator "Review.ImportSimple"
        initialContextCreator
        |> Review.Rule.providesFixesForModuleRule
        |> Review.Rule.withImportVisitor
            (\(Elm.Syntax.Node.Node range import_) context ->
                ( []
                , { context
                    | imports =
                        context.imports
                            |> FastDict.insert (import_.moduleName |> Elm.Syntax.Node.value)
                                { row = range.start.row
                                , isMatchingTypeExposed =
                                    isTypeExposedFrom import_.exposingList
                                        (import_.moduleName |> Elm.Syntax.Node.value |> String.concat)
                                }
                  }
                )
            )
        |> Review.Rule.withDeclarationListVisitor
            (\declarationList context ->
                ( []
                , { context
                    | references =
                        declarationList
                            |> List.map Elm.Syntax.Node.value
                            |> Declaration.LocalExtra.listReferences
                  }
                )
            )
        |> Review.Rule.withFinalModuleEvaluation
            (\context ->
                case context.references |> List.filterMap (referenceFixQualification context.originLookup) of
                    [] ->
                        []

                    fixableReference0 :: fixableReference1Up ->
                        let
                            importList : List { moduleName : Elm.Syntax.ModuleName.ModuleName, row : Int, isMatchingTypeExposed : Bool }
                            importList =
                                context.imports
                                    |> FastDict.toList
                                    |> List.map
                                        (\( moduleName, importInfo ) ->
                                            { moduleName = moduleName
                                            , row = importInfo.row
                                            , isMatchingTypeExposed = importInfo.isMatchingTypeExposed
                                            }
                                        )

                            errorRange : Elm.Syntax.Range.Range
                            errorRange =
                                case importList of
                                    firstFixableImport :: _ ->
                                        -- [import]
                                        { start = { row = firstFixableImport.row, column = 1 }
                                        , end = { row = firstFixableImport.row, column = 7 }
                                        }

                                    -- we already have fixable references, so this should not happen
                                    [] ->
                                        -- [module]
                                        { start = { row = 1, column = 1 }
                                        , end = { row = 1, column = 7 }
                                        }
                        in
                        [ Review.Rule.errorWithFix
                            { message = "The imports aren't simple"
                            , details =
                                [ "Each import should be either import Module.Name or import Module.Name exposing (ModuleName). This ensures consistency across files and makes reference origins obvious."
                                , "I suggest fixing this by fully qualifying the references or applying the automatic fix."
                                ]
                            }
                            errorRange
                            ((importList
                                |> List.map
                                    (\fixable ->
                                        Review.Fix.replaceRangeBy
                                            (fixable.row |> lineRange)
                                            (fixable |> simpleImportToString)
                                    )
                             )
                                ++ ((fixableReference0 :: fixableReference1Up)
                                        |> List.map
                                            (\fixable ->
                                                Review.Fix.replaceRangeBy fixable.range (fixable |> qualifiedToString)
                                            )
                                   )
                            )
                        ]
            )
        |> Review.Rule.fromModuleRuleSchema


isTypeExposedFrom : Maybe (Elm.Syntax.Node.Node Elm.Syntax.Exposing.Exposing) -> (String -> Bool)
isTypeExposedFrom exposingList =
    \typeNameToFind ->
        case exposingList |> Maybe.map Elm.Syntax.Node.value of
            Nothing ->
                False

            Just (Elm.Syntax.Exposing.All _) ->
                -- TODO potentially see actual exposes
                False

            Just (Elm.Syntax.Exposing.Explicit exposes) ->
                exposes
                    |> List.any
                        (\(Elm.Syntax.Node.Node _ expose) ->
                            case expose of
                                Elm.Syntax.Exposing.InfixExpose _ ->
                                    False

                                Elm.Syntax.Exposing.FunctionExpose _ ->
                                    False

                                Elm.Syntax.Exposing.TypeOrAliasExpose typeOrAliasExpose ->
                                    typeOrAliasExpose == typeNameToFind

                                Elm.Syntax.Exposing.TypeExpose choiceTypeExpose ->
                                    case choiceTypeExpose.open of
                                        Just _ ->
                                            False

                                        Nothing ->
                                            choiceTypeExpose.name == typeNameToFind
                        )


qualifiedToString : { a | qualification : List String, name : String } -> String
qualifiedToString =
    \qualified ->
        (qualified.qualification |> String.join ".")
            ++ "."
            ++ qualified.name


referenceFixQualification :
    Review.ModuleNameLookupTable.ModuleNameLookupTable
    -> { lookupRange : Elm.Syntax.Range.Range, range : Elm.Syntax.Range.Range, moduleName : Elm.Syntax.ModuleName.ModuleName, name : String }
    -> Maybe { range : Elm.Syntax.Range.Range, qualification : Elm.Syntax.ModuleName.ModuleName, name : String }
referenceFixQualification originLookup =
    \reference ->
        if (reference.moduleName == []) && (reference.name == "List") then
            Nothing

        else
            case Review.ModuleNameLookupTable.moduleNameAt originLookup reference.lookupRange of
                Nothing ->
                    Nothing

                Just [] ->
                    Nothing

                Just (moduleNamePart0 :: moduleNamePart1Up) ->
                    case implicitImports |> FastDict.get (moduleNamePart0 :: moduleNamePart1Up) of
                        Nothing ->
                            if reference.moduleName == (moduleNamePart0 :: moduleNamePart1Up) then
                                Nothing

                            else if
                                (reference.moduleName == [])
                                    && (reference.name == ((moduleNamePart0 :: moduleNamePart1Up) |> String.concat))
                            then
                                Nothing

                            else
                                { range = reference.range
                                , qualification = moduleNamePart0 :: moduleNamePart1Up
                                , name = reference.name
                                }
                                    |> Just

                        Just implicitImportInfo ->
                            case implicitImportInfo.alias of
                                Just implicitAlias ->
                                    if reference.moduleName == [ implicitAlias ] then
                                        Nothing

                                    else if
                                        (reference.moduleName == [])
                                            && (implicitImportInfo.exposed |> Set.member reference.name)
                                    then
                                        Nothing

                                    else
                                        { range = reference.range
                                        , qualification = [ implicitAlias ]
                                        , name = reference.name
                                        }
                                            |> Just

                                Nothing ->
                                    if reference.moduleName == (moduleNamePart0 :: moduleNamePart1Up) then
                                        Nothing

                                    else if implicitImportInfo.exposed |> Set.member reference.name then
                                        Nothing

                                    else
                                        { range = reference.range
                                        , qualification = moduleNamePart0 :: moduleNamePart1Up
                                        , name = reference.name
                                        }
                                            |> Just


lineRange : Int -> Elm.Syntax.Range.Range
lineRange row =
    { start = { row = row, column = 1 }
    , end = { row = row + 1, column = 1 }
    }


simpleImportToString : { a | moduleName : List String, isMatchingTypeExposed : Bool } -> String
simpleImportToString =
    \simpleImport ->
        "import "
            ++ (simpleImport.moduleName |> String.join ".")
            ++ (if simpleImport.isMatchingTypeExposed then
                    " exposing (" ++ (simpleImport.moduleName |> String.concat) ++ ")"

                else
                    ""
               )
            ++ "\n"


type alias Context =
    { originLookup : Review.ModuleNameLookupTable.ModuleNameLookupTable
    , imports :
        FastDict.Dict
            Elm.Syntax.ModuleName.ModuleName
            { row : Int
            , isMatchingTypeExposed : Bool
            }
    , references : List { lookupRange : Elm.Syntax.Range.Range, range : Elm.Syntax.Range.Range, moduleName : Elm.Syntax.ModuleName.ModuleName, name : String }
    }


initialContextCreator : Review.Rule.ContextCreator () Context
initialContextCreator =
    Review.Rule.initContextCreator
        (\originLookup () ->
            { originLookup = originLookup
            , imports = FastDict.empty
            , references = []
            }
        )
        |> Review.Rule.withModuleNameLookupTable


{-| From the `elm/core` readme:

>
> ### Default Imports

> The modules in this package are so common, that some of them are imported by default in all Elm files. So it is as if every Elm file starts with these imports:
>
>     import Basics exposing (..)
>     import List exposing (List, (::))
>     import Maybe exposing (Maybe(..))
>     import Result exposing (Result(..))
>     import String exposing (String)
>     import Char exposing (Char)
>     import Tuple
>     import Debug
>     import Platform exposing (Program)
>     import Platform.Cmd as Cmd exposing (Cmd)
>     import Platform.Sub as Sub exposing (Sub)

-}
implicitImports :
    FastDict.Dict
        Elm.Syntax.ModuleName.ModuleName
        { alias : Maybe String
        , exposed : Set String -- includes names of variants
        }
implicitImports =
    [ ( [ "Basics" ]
      , { alias = Nothing
        , exposed =
            [ "Int"
            , "Float"
            , "+"
            , "-"
            , "*"
            , "/"
            , "//"
            , "^"
            , "toFloat"
            , "round"
            , "floor"
            , "ceiling"
            , "truncate"
            , "=="
            , "/="
            , "<"
            , ">"
            , "<="
            , ">="
            , "max"
            , "min"
            , "compare"
            , "Order"
            , "LT"
            , "EQ"
            , "GT"
            , "Bool"
            , "True"
            , "False"
            , "not"
            , "&&"
            , "||"
            , "xor"
            , "++"
            , "modBy"
            , "remainderBy"
            , "negate"
            , "abs"
            , "clamp"
            , "sqrt"
            , "logBase"
            , "e"
            , "pi"
            , "cos"
            , "sin"
            , "tan"
            , "acos"
            , "asin"
            , "atan"
            , "atan2"
            , "degrees"
            , "radians"
            , "turns"
            , "toPolar"
            , "fromPolar"
            , "isNaN"
            , "isInfinite"
            , "identity"
            , "always"
            , "<|"
            , "|>"
            , "<<"
            , ">>"
            , "Never"
            , "never"
            ]
                |> Set.fromList
        }
      )
    , -- exposing (List) excluded because List.List is not valid.
      -- List is not exposed from module List
      -- and is instead provided through compiler magic
      ( [ "List" ], { alias = Nothing, exposed = Set.singleton "(::)" } )
    , ( [ "Maybe" ], { alias = Nothing, exposed = Set.fromList [ "Maybe", "Just", "Nothing" ] } )
    , ( [ "Result" ], { alias = Nothing, exposed = Set.fromList [ "Result", "Ok", "Err" ] } )
    , ( [ "String" ], { alias = Nothing, exposed = Set.singleton "String" } )
    , ( [ "Char" ], { alias = Nothing, exposed = Set.singleton "Char" } )
    , ( [ "Tuple" ], { alias = Nothing, exposed = Set.empty } )
    , ( [ "Debug" ], { alias = Nothing, exposed = Set.empty } )
    , ( [ "Platform" ], { alias = Nothing, exposed = Set.singleton "Program" } )
    , ( [ "Platform", "Cmd" ], { alias = Just "Cmd", exposed = Set.singleton "Cmd" } )
    , ( [ "Platform", "Sub" ], { alias = Just "Sub", exposed = Set.singleton "Sub" } )
    ]
        |> FastDict.fromList
