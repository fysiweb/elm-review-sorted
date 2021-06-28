module RulesTest exposing (all)

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
            [ test "should report an error when record fields are unsorted" <|
                \() ->
                    """module A exposing (..)
type alias Record = { b : Int, a : Int }

type alias RecordTwo = { c : Bool }

type WrappedRecord = Record { blue : String, black : { one : Bool, two : Bool, three : Bool } }

getC : RecordTwo -> Bool
getC {c} = c

getB : { t | b : Int, a : Int }
getB {b, a} = b

makeRecordTwo : Bool -> RecordTwo
makeRecordTwo c = {c = c}
"""
                        |> Review.Test.run NoUnsortedRecordFields.rule
                        |> Review.Test.expectErrors
                            [ unsortedRecordFieldsError "{ b : Int, a : Int }"
                            , unsortedRecordFieldsError "{ blue : String, black : { one : Bool, two : Bool, three : Bool } }"
                            , unsortedRecordFieldsError " b : Int, a : Int "
                                |> Review.Test.atExactly { start = { row = 11, column = 13 }, end = { row = 11, column = 31 } }
                            , unsortedRecordFieldsError "{b, a}"
                            ]
            ]
        , describe "NoUnsortedConstructors"
            [ test "should report an error when constructors are unsorted" <|
                \() ->
                    """module B exposing (..)
type Alternative = C | B | A

toString : Maybe Alternative -> String
toString alternative = case alternative of
    Nothing -> ""
    Just B -> "B"
    Just A -> "A"
    Just C -> "C" """
                        |> Review.Test.run NoUnsortedConstructors.rule
                        |> Review.Test.expectErrors
                            [ unsortedConstructorsError "type Alternative = C | B | A"
                            , unsortedConstructorsError """case alternative of
    Nothing -> ""
    Just B -> "B"
    Just A -> "A"
    Just C -> "C" """
                            ]
            ]
        ]
