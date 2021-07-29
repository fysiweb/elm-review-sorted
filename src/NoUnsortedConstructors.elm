module NoUnsortedConstructors exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Declaration exposing (Declaration)
import Elm.Syntax.Expression exposing (Expression)
import Elm.Syntax.Node exposing (Node)
import Elm.Syntax.Pattern
import Elm.Syntax.Range exposing (Range)
import Review.Fix
import Review.Rule exposing (Error, Rule)
import Util exposing (declarationToFix, expressionToFix, stableSort)


unsortedError : Range -> String -> Error {}
unsortedError range fix =
    Review.Rule.errorWithFix
        { message = "Sort constructors."
        , details = [ "Constructors have to be sorted in some way, why not alphabetically?" ]
        }
        range
        [ Review.Fix.replaceRangeBy range fix ]


{-| Reports...

    config =
        [ NoUnsortedConstructors.rule
        ]


## Fail

    data Foo = Baz | Bar | Quux


## Success

    data Foo = Bar | Baz | Quux


## When (not) to enable this rule

This rule is useful when you want to enforce sorting type constructors.
THis rule is not useful when you don't.

-}
rule : Rule
rule =
    Review.Rule.newModuleRuleSchema "NoUnsortedConstructors" ()
        |> Review.Rule.withSimpleExpressionVisitor expressionVisitor
        |> Review.Rule.withSimpleDeclarationVisitor declarationVisitor
        |> Review.Rule.fromModuleRuleSchema


declarationVisitor : Node Declaration -> List (Error {})
declarationVisitor node =
    case Elm.Syntax.Node.value node of
        Elm.Syntax.Declaration.CustomTypeDeclaration type_ ->
            let
                compareConstructors d1 d2 =
                    compare
                        (Elm.Syntax.Node.value d1
                            |> .name
                            |> Elm.Syntax.Node.value
                        )
                        (Elm.Syntax.Node.value d2
                            |> .name
                            |> Elm.Syntax.Node.value
                        )
            in
            stableSort compareConstructors type_.constructors
                |> Maybe.map
                    (\sorted ->
                        let
                            range =
                                Elm.Syntax.Node.range node
                        in
                        Elm.Syntax.Declaration.CustomTypeDeclaration { type_ | constructors = sorted }
                            |> declarationToFix range
                            |> unsortedError range
                            |> List.singleton
                    )
                |> Maybe.withDefault []

        _ ->
            []


expressionVisitor : Node Expression -> List (Error {})
expressionVisitor node =
    case Elm.Syntax.Node.value node of
        Elm.Syntax.Expression.CaseExpression ({ cases } as r) ->
            let
                comparePatterns p1 p2 =
                    case ( p1, p2 ) of
                        ( Elm.Syntax.Pattern.NamedPattern name1 ps1, Elm.Syntax.Pattern.NamedPattern name2 ps2 ) ->
                            case compare name1.name name2.name of
                                EQ ->
                                    List.map2 (\x y -> comparePatterns (Elm.Syntax.Node.value x) (Elm.Syntax.Node.value y)) ps1 ps2
                                        |> List.filter (\x -> x /= EQ)
                                        |> List.head
                                        |> Maybe.withDefault EQ

                                order ->
                                    order

                        ( _, _ ) ->
                            EQ

                compareCase ( p1, _ ) ( p2, _ ) =
                    comparePatterns (Elm.Syntax.Node.value p1) (Elm.Syntax.Node.value p2)
            in
            stableSort compareCase cases
                |> Maybe.map
                    (\sorted ->
                        let
                            range =
                                Elm.Syntax.Node.range node
                        in
                        Elm.Syntax.Expression.CaseExpression { r | cases = sorted }
                            |> expressionToFix range
                            |> unsortedError range
                            |> List.singleton
                    )
                |> Maybe.withDefault []

        _ ->
            []
