module NoUnsortedConstructors exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Declaration exposing (Declaration)
import Elm.Syntax.Expression exposing (Expression)
import Elm.Syntax.Node exposing (Node)
import Elm.Syntax.Pattern exposing (Pattern)
import Elm.Syntax.Type exposing (Type)
import Review.Rule exposing (Error, Rule)


unsortedError =
    Review.Rule.error
        { message = "Sort constructors."
        , details = [ "Constructors have to be sorted in some way, why not alphabetically?" ]
        }


{-| Reports...

    config =
        [ NoUnsortedConstructors.rule
        ]


## Fail

    TODO


## Success

    TODO


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
                constructors =
                    List.map (Elm.Syntax.Node.value >> .name >> Elm.Syntax.Node.value) type_.constructors
            in
            if List.sort constructors == constructors then
                []

            else
                Elm.Syntax.Node.range node
                    |> unsortedError
                    |> List.singleton

        _ ->
            []


expressionVisitor : Node Expression -> List (Error {})
expressionVisitor node =
    case Elm.Syntax.Node.value node of
        Elm.Syntax.Expression.CaseExpression { cases } ->
            let
                patterns =
                    List.map (Tuple.first >> Elm.Syntax.Node.value) cases

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
            in
            if List.sortWith comparePatterns patterns == patterns then
                []

            else
                Elm.Syntax.Node.range node |> unsortedError |> List.singleton

        _ ->
            []
