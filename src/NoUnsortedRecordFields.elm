module NoUnsortedRecordFields exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Declaration exposing (Declaration)
import Elm.Syntax.Expression exposing (Expression, Function, RecordSetter)
import Elm.Syntax.Node exposing (Node)
import Elm.Syntax.Pattern exposing (Pattern)
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.TypeAnnotation exposing (RecordDefinition, TypeAnnotation)
import Review.Fix
import Review.Rule exposing (Error, Rule)
import Util exposing (expressionToFix, patternToFix, stableSort, typeAnnotationToFix)


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

    sumFoo : Foo -> Int
    sumFoo { b, a } =
        a + b


## Success

    type alias Foo =
        { a : Int, b : Int }

    foo : Foo
    foo =
        { a = 2, b = 1 }

    sumFoo : Foo -> Int
    sumFoo { a, b } =
        a + b


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


unsortedError : Range -> String -> Error {}
unsortedError range fix =
    Review.Rule.errorWithFix
        { message = "Sort record fields."
        , details = [ "Record fields have to be sorted in some way, why not alphabetically?" ]
        }
        range
        [ Review.Fix.replaceRangeBy range fix ]


declarationVisitor : Node Declaration -> List (Error {})
declarationVisitor node =
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
        recordSettersSorted :
            (List (Node RecordSetter) -> Expression)
            -> List (Node RecordSetter)
            -> List (Error {})
        recordSettersSorted expr recordSetters =
            let
                compareSetters d1 d2 =
                    compare
                        (Elm.Syntax.Node.value d1
                            |> Tuple.first
                            |> Elm.Syntax.Node.value
                        )
                        (Elm.Syntax.Node.value d2
                            |> Tuple.first
                            |> Elm.Syntax.Node.value
                        )
            in
            (stableSort compareSetters recordSetters
                |> Maybe.map
                    (\sorted ->
                        let
                            range =
                                Elm.Syntax.Node.range node
                        in
                        expr sorted
                            |> expressionToFix range
                            |> unsortedError range
                            |> List.singleton
                    )
                |> Maybe.withDefault []
            )
                ++ List.concatMap
                    (Elm.Syntax.Node.value
                        >> Tuple.second
                        >> expressionVisitor
                    )
                    recordSetters

        letDeclarationSorted declaration =
            case Elm.Syntax.Node.value declaration of
                Elm.Syntax.Expression.LetFunction function ->
                    functionSorted function

                Elm.Syntax.Expression.LetDestructuring pattern _ ->
                    patternSorted pattern
    in
    case Elm.Syntax.Node.value node of
        Elm.Syntax.Expression.RecordExpr recordSetters ->
            recordSettersSorted Elm.Syntax.Expression.RecordExpr recordSetters

        Elm.Syntax.Expression.RecordUpdateExpression name recordSetters ->
            recordSettersSorted (Elm.Syntax.Expression.RecordUpdateExpression name) recordSetters

        Elm.Syntax.Expression.LambdaExpression { args } ->
            List.concatMap patternSorted args

        Elm.Syntax.Expression.LetExpression { declarations } ->
            List.concatMap letDeclarationSorted declarations

        Elm.Syntax.Expression.CaseExpression { cases } ->
            List.concatMap (patternSorted << Tuple.first) cases

        _ ->
            []


{-| Helper to check that record fields in a type annotation are sorted, if any
exist.
-}
typeAnnotationSorted : Node TypeAnnotation -> List (Error {})
typeAnnotationSorted typeAnnotation =
    let
        recordDefinitionSorted :
            (RecordDefinition -> TypeAnnotation)
            -> RecordDefinition
            -> List (Error {})
        recordDefinitionSorted annot fields =
            let
                compareFields d1 d2 =
                    compare
                        (Elm.Syntax.Node.value d1
                            |> Tuple.first
                            |> Elm.Syntax.Node.value
                        )
                        (Elm.Syntax.Node.value d2
                            |> Tuple.first
                            |> Elm.Syntax.Node.value
                        )
            in
            stableSort compareFields fields
                |> Maybe.map
                    (\sorted ->
                        let
                            range =
                                Elm.Syntax.Node.range typeAnnotation
                        in
                        annot sorted
                            |> typeAnnotationToFix range
                            |> unsortedError range
                            |> List.singleton
                    )
                |> Maybe.withDefault []
    in
    case Elm.Syntax.Node.value typeAnnotation of
        Elm.Syntax.TypeAnnotation.Typed _ annotations ->
            List.concatMap typeAnnotationSorted annotations

        Elm.Syntax.TypeAnnotation.Tupled annotations ->
            List.concatMap typeAnnotationSorted annotations

        Elm.Syntax.TypeAnnotation.Record record ->
            recordDefinitionSorted Elm.Syntax.TypeAnnotation.Record record

        Elm.Syntax.TypeAnnotation.GenericRecord name record ->
            Elm.Syntax.Node.value record
                |> recordDefinitionSorted (\def -> Elm.Syntax.TypeAnnotation.GenericRecord name (Elm.Syntax.Node.map (always def) record))

        Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation input output ->
            typeAnnotationSorted input ++ typeAnnotationSorted output

        _ ->
            []


{-| Helper to check that a function's type annotation and arguments are sorted,
if they contain record fields.
-}
functionSorted : Function -> List (Error {})
functionSorted { declaration, signature } =
    Maybe.withDefault [] (Maybe.map typeAnnotationSorted (Maybe.map (Elm.Syntax.Node.value >> .typeAnnotation) signature))
        ++ List.concatMap patternSorted (Elm.Syntax.Node.value declaration).arguments


{-| Helper to check that a pattern is sorted if it is a record pattern, used by
both `declarationVisitor` and `expressionVisitor`.
-}
patternSorted : Node Pattern -> List (Error {})
patternSorted pattern =
    case Elm.Syntax.Node.value pattern of
        Elm.Syntax.Pattern.RecordPattern record ->
            let
                compareRecord d1 d2 =
                    compare
                        (Elm.Syntax.Node.value d1)
                        (Elm.Syntax.Node.value d2)
            in
            stableSort compareRecord record
                |> Maybe.map
                    (\sorted ->
                        let
                            range =
                                Elm.Syntax.Node.range pattern
                        in
                        Elm.Syntax.Pattern.RecordPattern sorted
                            |> patternToFix range
                            |> unsortedError range
                            |> List.singleton
                    )
                |> Maybe.withDefault []

        _ ->
            []
