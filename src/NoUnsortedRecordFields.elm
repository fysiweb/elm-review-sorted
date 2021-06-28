module NoUnsortedRecordFields exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Declaration exposing (Declaration)
import Elm.Syntax.Expression exposing (Expression)
import Elm.Syntax.Node exposing (Node)
import Elm.Syntax.Pattern exposing (Pattern)
import Elm.Syntax.Signature exposing (Signature)
import Elm.Syntax.Type exposing (Type)
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation)
import Review.Rule exposing (Error, Rule)


{-| Reports...

    config =
        [ NoUnsortedRecordFields.rule
        ]


## Fail

    type alias Foo =
        { b : Int, a : Int }

    foo : Foo
    foo =
        { b = 1, a = 2 }


## Success

    type alias Foo =
        { a : Int, b : Int }

    foo : Foo
    foo =
        { a = 2, b = 1 }


## When (not) to enable this rule

This rule is useful when you want to enforce sorting in record fields.
THis rule is not useful when you don't.

-}
rule : Rule
rule =
    Review.Rule.newModuleRuleSchema "NoUnsortedRecordFields" ()
        |> Review.Rule.withSimpleExpressionVisitor expressionVisitor
        |> Review.Rule.withSimpleDeclarationVisitor declarationVisitor
        |> Review.Rule.fromModuleRuleSchema


unsortedError =
    Review.Rule.error
        { message = "Sort record fields."
        , details = [ "Record fields have to be sorted in some way, why not alphabetically?" ]
        }


declarationVisitor : Node Declaration -> List (Error {})
declarationVisitor node =
    let
        recordDefinitionSorted recordDefinition =
            let
                fields =
                    Elm.Syntax.Node.value recordDefinition
                        |> List.map (Elm.Syntax.Node.value >> Tuple.first >> Elm.Syntax.Node.value)
            in
            if List.sort fields == fields then
                []

            else
                Elm.Syntax.Node.range recordDefinition |> unsortedError |> List.singleton

        patternSorted pattern =
            case Elm.Syntax.Node.value pattern of
                Elm.Syntax.Pattern.RecordPattern record ->
                    let
                        fields =
                            List.map Elm.Syntax.Node.value record
                    in
                    if List.sort fields == fields then
                        []

                    else
                        Elm.Syntax.Node.range pattern
                            |> unsortedError
                            |> List.singleton

                _ ->
                    []

        typeAnnotationSorted typeAnnotation =
            case Elm.Syntax.Node.value typeAnnotation of
                Elm.Syntax.TypeAnnotation.Typed _ annotations ->
                    List.concatMap typeAnnotationSorted annotations

                Elm.Syntax.TypeAnnotation.Tupled annotations ->
                    List.concatMap typeAnnotationSorted annotations

                Elm.Syntax.TypeAnnotation.Record record ->
                    recordDefinitionSorted (Elm.Syntax.Node.map (always record) typeAnnotation)

                Elm.Syntax.TypeAnnotation.GenericRecord _ record ->
                    recordDefinitionSorted record

                Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation input output ->
                    typeAnnotationSorted input ++ typeAnnotationSorted output

                _ ->
                    []

        functionSorted { declaration, signature } =
            Maybe.withDefault [] (Maybe.map typeAnnotationSorted (Maybe.map (Elm.Syntax.Node.value >> .typeAnnotation) signature))
                ++ List.concatMap patternSorted (Elm.Syntax.Node.value declaration).arguments

        letDeclarationSorted declaration =
            case Elm.Syntax.Node.value declaration of
                Elm.Syntax.Expression.LetFunction function ->
                    functionSorted function

                Elm.Syntax.Expression.LetDestructuring pattern _ ->
                    patternSorted pattern
    in
    case Elm.Syntax.Node.value node of
        Elm.Syntax.Declaration.FunctionDeclaration function ->
            functionSorted function

        Elm.Syntax.Declaration.AliasDeclaration { typeAnnotation } ->
            typeAnnotationSorted typeAnnotation

        Elm.Syntax.Declaration.CustomTypeDeclaration { constructors } ->
            constructors
                |> List.concatMap
                    (Elm.Syntax.Node.value
                        >> .arguments
                        >> List.concatMap typeAnnotationSorted
                    )

        Elm.Syntax.Declaration.Destructuring pattern _ ->
            patternSorted pattern

        _ ->
            []


expressionVisitor : Node Expression -> List (Error {})
expressionVisitor node =
    let
        recordSettersSorted recordSetters =
            let
                fields =
                    List.map (Elm.Syntax.Node.value >> Tuple.first >> Elm.Syntax.Node.value) (Elm.Syntax.Node.value recordSetters)
            in
            (if fields == List.sort fields then
                []

             else
                Elm.Syntax.Node.range recordSetters |> unsortedError |> List.singleton
            )
                ++ List.concatMap (Elm.Syntax.Node.value >> Tuple.second >> expressionVisitor) (Elm.Syntax.Node.value recordSetters)
    in
    case Elm.Syntax.Node.value node of
        Elm.Syntax.Expression.RecordExpr recordSetters ->
            recordSettersSorted (Elm.Syntax.Node.map (always recordSetters) node)

        Elm.Syntax.Expression.RecordUpdateExpression _ recordSetters ->
            recordSettersSorted (Elm.Syntax.Node.map (always recordSetters) node)

        _ ->
            []
