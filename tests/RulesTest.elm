module RulesTest exposing (all)

-- ! NOTE: Do not strip trailing whitespace in multi-line strings, or tests
-- ! will fail; your IDE may do this automatically.

import NoUnsortedConstructors
import NoUnsortedRecordFields
import Review.Test
import Test exposing (Test, describe, test)


unsortedRecordFieldsError : String -> Review.Test.ExpectedError
unsortedRecordFieldsError string =
    Review.Test.error
        { message = "Sort record fields."
        , details = [ "Record fields have to be sorted in some way, why not alphabetically?" ]
        , under = string
        }


unsortedConstructorsError : String -> Review.Test.ExpectedError
unsortedConstructorsError string =
    Review.Test.error
        { message = "Sort constructors."
        , details = [ "Constructors have to be sorted in some way, why not alphabetically?" ]
        , under = string
        }


all : Test
all =
    describe "elm-review-sorted"
        [ describe "NoUnsortedRecordFields"
            [ test "unsorted declaration" <|
                \() ->
                    """module A exposing (..)

type alias Record = { b : Int, a : Int }
"""
                        |> Review.Test.run NoUnsortedRecordFields.rule
                        |> Review.Test.expectErrors
                            [ unsortedRecordFieldsError "{ b : Int, a : Int }"
                                |> Review.Test.whenFixed
                                    """module A exposing (..)

type alias Record = {a : Int, b : Int}
"""
                            ]
            , test "no error for single fields" <|
                \() ->
                    """module A exposing (..)

type alias RecordTwo = { c : Bool }

getC : RecordTwo -> Bool
getC {c} = c

makeRecordTwo : Bool -> RecordTwo
makeRecordTwo c = {c = c}
"""
                        |> Review.Test.run NoUnsortedRecordFields.rule
                        |> Review.Test.expectNoErrors
            , test "no error for sorted" <|
                \() ->
                    """module A exposing (..)

type alias Record = { a : Int, b : Int }

type WrappedRecord = Record { black : { one : Bool, two : Bool, three : Bool }, blue : String }

getB : { t | a : Int, b : Int }
getB {a, b} = b

lambdaFunction : Record -> Int
lambdaFunction = \\{a, b} -> b

letDestructuring : Record -> Int
letDestructuring r =
    let {a, b} = r
    in b

letFuncSigAndDestructuring : Record -> Int
letFuncSigAndDestructuring r =
    let
        f : { t | a : Int, b : Int } -> Int
        f {a, b} = b
    in
    f r

casePatternMatch : Record -> Int
casePatternMatch r =
    case r of
        {a, b} -> b
"""
                        |> Review.Test.run NoUnsortedRecordFields.rule
                        |> Review.Test.expectNoErrors
            , test "nested records" <|
                \() ->
                    """module A exposing (..)

type WrappedRecord = Record { blue : String, black : { one : Bool, two : Bool, three : Bool } }
"""
                        |> Review.Test.run NoUnsortedRecordFields.rule
                        |> Review.Test.expectErrors
                            [ unsortedRecordFieldsError "{ blue : String, black : { one : Bool, two : Bool, three : Bool } }"
                                |> Review.Test.whenFixed
                                    """module A exposing (..)

type WrappedRecord = Record {black : {one : Bool, two : Bool, three : Bool}, blue : String}
"""
                            ]
            , test "generic record and argument destructure" <|
                \() ->
                    """module A exposing (..)

getB : { t | b : Int, a : Int }
getB {b, a} = b
"""
                        |> Review.Test.run NoUnsortedRecordFields.rule
                        |> Review.Test.expectErrors
                            [ unsortedRecordFieldsError "{ t | b : Int, a : Int }"
                                |> Review.Test.whenFixed
                                    """module A exposing (..)

getB : { t | a : Int, b : Int }
getB {b, a} = b
"""
                            , unsortedRecordFieldsError "{b, a}"
                                |> Review.Test.whenFixed
                                    """module A exposing (..)

getB : { t | b : Int, a : Int }
getB {a, b} = b
"""
                            ]
            , test "lamda function" <|
                \() ->
                    """module A exposing (..)

type alias Record = { a : Int, b : Int }

lambdaFunction : Record -> Int
lambdaFunction = \\{b, a} -> b
"""
                        |> Review.Test.run NoUnsortedRecordFields.rule
                        |> Review.Test.expectErrors
                            [ unsortedRecordFieldsError "{b, a}"
                                |> Review.Test.whenFixed """module A exposing (..)

type alias Record = { a : Int, b : Int }

lambdaFunction : Record -> Int
lambdaFunction = \\{a, b} -> b
"""
                            ]
            , test "let destructuring" <|
                \() ->
                    """module A exposing (..)

type alias Record = { a : Int, b : Int }

letDestructuring : Record -> Int
letDestructuring r =
    let {b, a} = r
    in b
"""
                        |> Review.Test.run NoUnsortedRecordFields.rule
                        |> Review.Test.expectErrors
                            [ unsortedRecordFieldsError "{b, a}"
                                |> Review.Test.whenFixed """module A exposing (..)

type alias Record = { a : Int, b : Int }

letDestructuring : Record -> Int
letDestructuring r =
    let {a, b} = r
    in b
"""
                            ]
            , test "function signature and argument destructuring in let binding" <|
                \() ->
                    """module A exposing (..)

type alias Record = { a : Int, b : Int }

letFuncSigAndDestructuring : Record -> Int
letFuncSigAndDestructuring r =
    let
        f : { t | b : Int, a : Int } -> Int
        f {b, a} = b
    in
    f r
"""
                        |> Review.Test.run NoUnsortedRecordFields.rule
                        |> Review.Test.expectErrors
                            [ unsortedRecordFieldsError "{ t | b : Int, a : Int }"
                                |> Review.Test.whenFixed """module A exposing (..)

type alias Record = { a : Int, b : Int }

letFuncSigAndDestructuring : Record -> Int
letFuncSigAndDestructuring r =
    let
        f : { t | a : Int, b : Int } -> Int
        f {b, a} = b
    in
    f r
"""
                            , unsortedRecordFieldsError "{b, a}"
                                |> Review.Test.whenFixed """module A exposing (..)

type alias Record = { a : Int, b : Int }

letFuncSigAndDestructuring : Record -> Int
letFuncSigAndDestructuring r =
    let
        f : { t | b : Int, a : Int } -> Int
        f {a, b} = b
    in
    f r
"""
                            ]
            , test "case pattern match" <|
                \() ->
                    """module A exposing (..)

type alias Record = { a : Int, b : Int }

casePatternMatch : Record -> Int
casePatternMatch r =
    case r of
        {b, a} -> b
"""
                        |> Review.Test.run NoUnsortedRecordFields.rule
                        |> Review.Test.expectErrors
                            [ unsortedRecordFieldsError "{b, a}"
                                |> Review.Test.whenFixed """module A exposing (..)

type alias Record = { a : Int, b : Int }

casePatternMatch : Record -> Int
casePatternMatch r =
    case r of
        {a, b} -> b
"""
                            ]
            , test "multiline signature in let" <|
                \() ->
                    """module A exposing (..)

type alias Record = { a : Int, b : Int }

letFuncSigAndDestructuring : Record -> Int
letFuncSigAndDestructuring r =
    let
        f :
            { t
                | b : Int
                , a : Int
            }
            -> Int
        f { a, b } =
            b
    in
    f r
"""
                        |> Review.Test.run NoUnsortedRecordFields.rule
                        |> Review.Test.expectErrors
                            [ unsortedRecordFieldsError """{ t
                | b : Int
                , a : Int
            }"""
                                |> Review.Test.whenFixed """module A exposing (..)

type alias Record = { a : Int, b : Int }

letFuncSigAndDestructuring : Record -> Int
letFuncSigAndDestructuring r =
    let
        f :
            { t | a : Int, b : Int }
            -> Int
        f { a, b } =
            b
    in
    f r
"""
                            ]
            ]
        , describe "NoUnsortedConstructors"
            [ test "unsorted declaration" <|
                \() ->
                    """module B exposing (..)
type Alternative = C | B | A
"""
                        |> Review.Test.run NoUnsortedConstructors.rule
                        |> Review.Test.expectErrors
                            [ unsortedConstructorsError "type Alternative = C | B | A" |> Review.Test.whenFixed """module B exposing (..)
type Alternative 
    =A |B |C 
"""
                            ]
            , test "unsorted declaration multiline" <|
                \() ->
                    """module B exposing (..)

type Multiline
    = Foo
    | Baz
    | Bar
"""
                        |> Review.Test.run NoUnsortedConstructors.rule
                        |> Review.Test.expectErrors
                            [ unsortedConstructorsError """type Multiline
    = Foo
    | Baz
    | Bar""" |> Review.Test.whenFixed """module B exposing (..)

type Multiline 
    =Bar 
    |Baz 
    |Foo 
"""
                            ]
            , test "unsorted case" <|
                \() ->
                    """module B exposing (..)

toString : Maybe Alternative -> String
toString alternative = case alternative of
    Nothing -> ""
    Just B -> "B"
    Just A -> "A"
    Just C -> "C" """
                        |> Review.Test.run NoUnsortedConstructors.rule
                        |> Review.Test.expectErrors
                            [ unsortedConstructorsError """case alternative of
    Nothing -> ""
    Just B -> "B"
    Just A -> "A"
    Just C -> "C" """ |> Review.Test.whenFixed """module B exposing (..)

toString : Maybe Alternative -> String
toString alternative = 
                       case alternative of
                         Just A  ->
                           "A"
                         Just B  ->
                           "B"
                         Just C  ->
                           "C"
                         Nothing  ->
                           ""
                        """
                            ]
            , test "should report an error when constructors are unsorted in a case" <|
                \() ->
                    """module B exposing (..)

toString : Maybe Alternative -> String
toString alternative = case alternative of
    Nothing -> List.map .x <| y
    Just B -> "B"
    Just A -> "A"
    Just C -> "C" """
                        |> Review.Test.run NoUnsortedConstructors.rule
                        |> Review.Test.expectErrors
                            [ unsortedConstructorsError """case alternative of
    Nothing -> List.map .x <| y
    Just B -> "B"
    Just A -> "A"
    Just C -> "C" """ |> Review.Test.whenFixed """module B exposing (..)

toString : Maybe Alternative -> String
toString alternative = 
                       case alternative of
                         Just A  ->
                           "A"
                         Just B  ->
                           "B"
                         Just C  ->
                           "C"
                         Nothing  ->
                           List.map .x <| y
                        """
                            ]
            ]
        ]
