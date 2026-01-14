module Review.ImportSimple exposing (rule)

{-| Allow only simple imports

@docs rule

-}

import Declaration.LocalExtra
import Elm.Syntax.Exposing
import Elm.Syntax.Import
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
                case context.importRange of
                    Nothing ->
                        []

                    Just importsRange ->
                        let
                            result :
                                { simpleImportsWithMatchingTypeNameIsUsed :
                                    FastDict.Dict Elm.Syntax.ModuleName.ModuleName Bool
                                , fixableReferences :
                                    List
                                        { range : Elm.Syntax.Range.Range
                                        , fixedQualification : Elm.Syntax.ModuleName.ModuleName
                                        , name : String
                                        }
                                }
                            result =
                                context.references
                                    |> List.foldl
                                        (\reference soFar ->
                                            case Review.ModuleNameLookupTable.moduleNameAt context.originLookup reference.lookupRange of
                                                Just [] ->
                                                    soFar

                                                maybeOriginModuleName ->
                                                    let
                                                        moduleOrigin : Elm.Syntax.ModuleName.ModuleName
                                                        moduleOrigin =
                                                            maybeOriginModuleName |> Maybe.withDefault reference.moduleName
                                                    in
                                                    { fixableReferences =
                                                        case reference |> referenceFixQualification moduleOrigin of
                                                            Nothing ->
                                                                soFar.fixableReferences

                                                            Just fixedReferenceQualification ->
                                                                { name = reference.name
                                                                , range = reference.range
                                                                , fixedQualification = fixedReferenceQualification
                                                                }
                                                                    :: soFar.fixableReferences
                                                    , simpleImportsWithMatchingTypeNameIsUsed =
                                                        if implicitImportsAccordingToModuleNameLookupTable |> FastDict.member moduleOrigin then
                                                            soFar.simpleImportsWithMatchingTypeNameIsUsed

                                                        else if
                                                            (reference.moduleName |> List.isEmpty)
                                                                && (reference.name == (moduleOrigin |> String.concat))
                                                        then
                                                            soFar.simpleImportsWithMatchingTypeNameIsUsed
                                                                |> FastDict.insert moduleOrigin
                                                                    True

                                                        else
                                                            soFar.simpleImportsWithMatchingTypeNameIsUsed
                                                                |> FastDict.update moduleOrigin
                                                                    (\maybeSimpleModuleOriginImportSoFar ->
                                                                        case maybeSimpleModuleOriginImportSoFar of
                                                                            Nothing ->
                                                                                Just False

                                                                            (Just _) as just ->
                                                                                just
                                                                    )
                                                    }
                                        )
                                        { simpleImportsWithMatchingTypeNameIsUsed = FastDict.empty
                                        , fixableReferences = []
                                        }

                            needsFixing : Bool
                            needsFixing =
                                Basics.not (List.isEmpty result.fixableReferences)
                                    || -- to catch e.g. every reference being good as is
                                       -- but from an import Array exposing (..)
                                       Basics.not
                                        (context.imports
                                            |> List.all (\(Elm.Syntax.Node.Node _ import_) -> import_ |> importIsSimple)
                                        )
                        in
                        if needsFixing then
                            [ Review.Rule.errorWithFix
                                { message = "The imports aren't simple"
                                , details =
                                    [ "Each import should be either import Module.Name or import Module.Name exposing (ModuleName). This ensures consistency across files and makes reference origins obvious."
                                    , "I suggest fixing this by fully qualifying the references or applying the automatic fix."
                                    ]
                                }
                                -- the import keyword
                                { start = { row = importsRange.start.row, column = 1 }
                                , end = { row = importsRange.start.row, column = 7 }
                                }
                                (Review.Fix.replaceRangeBy
                                    importsRange
                                    (result.simpleImportsWithMatchingTypeNameIsUsed
                                        |> FastDict.foldl
                                            (\moduleName matchingTypeIsUsed soFar ->
                                                soFar
                                                    ++ "\n"
                                                    ++ simpleImportToString
                                                        { moduleName = moduleName
                                                        , exposingMatchingType = matchingTypeIsUsed
                                                        }
                                            )
                                            ""
                                        |> String.dropLeft 1
                                    )
                                    :: (result.fixableReferences
                                            |> List.map
                                                (\fixable ->
                                                    Review.Fix.replaceRangeBy fixable.range
                                                        (qualifiedToString
                                                            { qualification = fixable.fixedQualification
                                                            , name = fixable.name
                                                            }
                                                        )
                                                )
                                       )
                                )
                            ]

                        else
                            []
            )
        |> Review.Rule.fromModuleRuleSchema


type alias Context =
    { originLookup : Review.ModuleNameLookupTable.ModuleNameLookupTable
    , importRange : Maybe Elm.Syntax.Range.Range
    , imports :
        List (Elm.Syntax.Node.Node Elm.Syntax.Import.Import)
    , references :
        List
            { lookupRange : Elm.Syntax.Range.Range
            , range : Elm.Syntax.Range.Range
            , moduleName : Elm.Syntax.ModuleName.ModuleName
            , name : String
            }
    }


initialContextCreator : Review.Rule.ContextCreator () Context
initialContextCreator =
    Review.Rule.initContextCreator
        (\fullAst originLookup () ->
            { originLookup = originLookup
            , importRange =
                case fullAst.imports of
                    [] ->
                        Nothing

                    import0 :: import1Up ->
                        Just
                            { start = import0 |> Elm.Syntax.Node.range |> .start
                            , end =
                                listFilledLast import0 import1Up
                                    |> Elm.Syntax.Node.range
                                    |> .end
                            }
            , imports = fullAst.imports
            , references = []
            }
        )
        |> Review.Rule.withFullAst
        |> Review.Rule.withModuleNameLookupTable


qualifiedToString : { qualification : List String, name : String } -> String
qualifiedToString qualified =
    (qualified.qualification |> String.join ".")
        ++ "."
        ++ qualified.name


referenceFixQualification :
    Elm.Syntax.ModuleName.ModuleName
    ->
        { lookupRange : Elm.Syntax.Range.Range
        , range : Elm.Syntax.Range.Range
        , moduleName : Elm.Syntax.ModuleName.ModuleName
        , name : String
        }
    -> Maybe Elm.Syntax.ModuleName.ModuleName
referenceFixQualification originModuleName reference =
    case implicitImportsAccordingToModuleNameLookupTable |> FastDict.get originModuleName of
        Nothing ->
            if reference.moduleName == originModuleName then
                -- already fully qualified
                Nothing

            else if
                List.isEmpty reference.moduleName
                    && (reference.name == (originModuleName |> String.concat))
            then
                -- allow exposed type/variant names that match the module name
                Nothing

            else
                Just originModuleName

        Just implicitImportInfo ->
            case implicitImportInfo.alias of
                Just implicitAlias ->
                    if
                        (reference.moduleName == [ implicitAlias ])
                            || (List.isEmpty reference.moduleName
                                    && (implicitImportInfo.exposed |> Set.member reference.name)
                               )
                    then
                        Nothing

                    else
                        Just [ implicitAlias ]

                Nothing ->
                    if
                        (reference.moduleName == originModuleName)
                            || (implicitImportInfo.exposed |> Set.member reference.name)
                    then
                        Nothing

                    else
                        Just originModuleName


simpleImportToString : { moduleName : List String, exposingMatchingType : Bool } -> String
simpleImportToString simpleImport =
    "import "
        ++ (simpleImport.moduleName |> String.join ".")
        ++ (if simpleImport.exposingMatchingType then
                " exposing (" ++ (simpleImport.moduleName |> String.concat) ++ ")"

            else
                ""
           )


importIsSimple : Elm.Syntax.Import.Import -> Bool
importIsSimple import_ =
    case import_.moduleAlias of
        Just _ ->
            False

        Nothing ->
            case import_.exposingList of
                Nothing ->
                    True

                Just (Elm.Syntax.Node.Node _ exposing_) ->
                    case exposing_ of
                        Elm.Syntax.Exposing.All _ ->
                            False

                        Elm.Syntax.Exposing.Explicit exposeList ->
                            case exposeList of
                                [ Elm.Syntax.Node.Node _ (Elm.Syntax.Exposing.TypeOrAliasExpose onlyExpose) ] ->
                                    onlyExpose == (Elm.Syntax.Node.value import_.moduleName |> String.concat)

                                _ ->
                                    False


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
implicitImportsAccordingToModuleNameLookupTable :
    FastDict.Dict
        Elm.Syntax.ModuleName.ModuleName
        { alias : Maybe String
        , exposed : Set String -- includes names of variants
        }
implicitImportsAccordingToModuleNameLookupTable =
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
    , ( [ "List" ], { alias = Nothing, exposed = Set.fromList [ "List", "(::)" ] } )
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


listFilledLast : a -> List a -> a
listFilledLast el0 el1Up =
    case el1Up of
        [] ->
            el0

        el1 :: el2Up ->
            listFilledLast el1 el2Up
