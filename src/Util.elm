module Util exposing (declarationToFix, expressionToFix, patternToFix, stableSort, typeAnnotationToFix)

import Elm.Syntax.Declaration exposing (Declaration)
import Elm.Syntax.Expression exposing (Expression)
import Elm.Syntax.Node
import Elm.Syntax.Pattern exposing (Pattern)
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation)
import Elm.Writer


{-| Sort a list stably and return the sorted version or `Nothing` if it is
already sorted. This does not require checking equality of `a`.
-}
stableSort : (a -> a -> Order) -> List a -> Maybe (List a)
stableSort comp xs =
    let
        withIndex =
            List.indexedMap Tuple.pair xs

        stableCompare ( i1, x1 ) ( i2, x2 ) =
            case comp x1 x2 of
                EQ ->
                    compare i1 i2

                ltOrGt ->
                    ltOrGt

        sorted =
            List.sortWith stableCompare withIndex
    in
    if List.map Tuple.first sorted == List.map Tuple.first withIndex then
        Nothing

    else
        Just (List.map Tuple.second sorted)


expressionToFix : Range -> Expression -> String
expressionToFix range e =
    Elm.Syntax.Node.Node range e
        |> Elm.Writer.writeExpression
        |> Elm.Writer.write
        |> reindent range.start.column


declarationToFix : Range -> Declaration -> String
declarationToFix range d =
    Elm.Syntax.Node.Node range d
        |> Elm.Writer.writeDeclaration
        |> Elm.Writer.write
        |> reindent range.start.column


patternToFix : Range -> Pattern -> String
patternToFix range p =
    Elm.Syntax.Node.Node range p
        |> Elm.Writer.writePattern
        |> Elm.Writer.write
        |> reindent range.start.column


typeAnnotationToFix : Range -> TypeAnnotation -> String
typeAnnotationToFix range a =
    Elm.Syntax.Node.Node range a
        |> Elm.Writer.writeTypeAnnotation
        |> Elm.Writer.write
        |> reindent range.start.column


reindent : Int -> String -> String
reindent amount =
    String.lines
        >> String.join ("\n" ++ String.repeat (amount - 1) " ")
