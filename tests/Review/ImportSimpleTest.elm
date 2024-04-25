module Review.ImportSimpleTest exposing (all)

import Review.ImportSimple
import Review.Test
import Test exposing (Test)


all : Test
all =
    Test.describe "Review.ImportSimple"
        [ Test.test "not report when imports only module name"
            (\() ->
                """module A exposing (..)
import Set

a =
    Set.empty
"""
                    |> Review.Test.run Review.ImportSimple.rule
                    |> Review.Test.expectNoErrors
            )
        , Test.test "not report when imports only module name exposing type with equal name"
            (\() ->
                """module A exposing (..)
import Set exposing (Set)

a : Set a_
a =
    Set.empty
"""
                    |> Review.Test.run Review.ImportSimple.rule
                    |> Review.Test.expectNoErrors
            )
        , Test.test "not report when imports only module name with . exposing type with equal name ignoring ."
            (\() ->
                """module A exposing (..)
import Set.Filled exposing (SetFilled)

a : SetFilled Int
a =
    Set.Filled.one 1
"""
                    |> Review.Test.run Review.ImportSimple.rule
                    |> Review.Test.expectNoErrors
            )
        , Test.test "not report when used import alias is implicit"
            (\() ->
                """module A exposing (..)

a =
    Sub.none
"""
                    |> Review.Test.run Review.ImportSimple.rule
                    |> Review.Test.expectNoErrors
            )
        , Test.test "not report when used unqualified reference is implicit"
            (\() ->
                """module A exposing (..)

a : Sub a_
a =
    Sub.none
"""
                    |> Review.Test.run Review.ImportSimple.rule
                    |> Review.Test.expectNoErrors
            )
        , Test.test "not report when used unqualified reference is List (edge case because List isn't exposed from module List)"
            (\() ->
                """module A exposing (..)

a : List a_
a =
    []
"""
                    |> Review.Test.run Review.ImportSimple.rule
                    |> Review.Test.expectNoErrors
            )
        , Test.test "report when function/value is exposed"
            (\() ->
                """module A exposing (..)
import Expect
import Test exposing (test)

a =
    test "passes" (\\() -> 1 |> Expect.equal 1)
"""
                    |> Review.Test.run Review.ImportSimple.rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The imports aren't simple"
                            , details =
                                [ "Each import should be either import Module.Name or import Module.Name exposing (ModuleName). This ensures consistency across files and makes reference origins obvious."
                                , "I suggest fixing this by fully qualifying the references or applying the automatic fix."
                                ]
                            , under = "import"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 1 }, end = { row = 2, column = 7 } }
                            |> Review.Test.whenFixed
                                """module A exposing (..)
import Expect
import Test

a =
    Test.test "passes" (\\() -> 1 |> Expect.equal 1)
"""
                        ]
            )
        , Test.test "report import alias"
            (\() ->
                """module A exposing (..)
import Test as T

a : T.Test
a =
    T.todo "a"
"""
                    |> Review.Test.run Review.ImportSimple.rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The imports aren't simple"
                            , details =
                                [ "Each import should be either import Module.Name or import Module.Name exposing (ModuleName). This ensures consistency across files and makes reference origins obvious."
                                , "I suggest fixing this by fully qualifying the references or applying the automatic fix."
                                ]
                            , under = "import"
                            }
                            |> Review.Test.whenFixed
                                """module A exposing (..)
import Test

a : Test.Test
a =
    Test.todo "a"
"""
                        ]
            )
        , Test.test "report when implicit import is aliased differently"
            (\() ->
                """module A exposing (..)
import Platform.Sub as Subscription

a =
    Subscription.none
"""
                    |> Review.Test.run Review.ImportSimple.rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The imports aren't simple"
                            , details =
                                [ "Each import should be either import Module.Name or import Module.Name exposing (ModuleName). This ensures consistency across files and makes reference origins obvious."
                                , "I suggest fixing this by fully qualifying the references or applying the automatic fix."
                                ]
                            , under = "import"
                            }
                            |> Review.Test.whenFixed
                                """module A exposing (..)
import Platform.Sub

a =
    Sub.none
"""
                        ]
            )
        ]
