module Util exposing (declarationToFix, expressionToFix, patternToFix, typeAnnotationToFix)

import Elm.Syntax.Declaration exposing (Declaration)
import Elm.Syntax.Expression exposing (Expression)
import Elm.Syntax.Node
import Elm.Syntax.Pattern exposing (Pattern)
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation)
import Elm.Writer


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
