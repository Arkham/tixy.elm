module ExpressionSpec exposing (spec)

import Expect
import Expression exposing (Expression(..))
import Parser
import Test exposing (..)


spec : Test
spec =
    describe "Expression"
        [ describe "digits"
            [ test "parses integers" <|
                \_ ->
                    parseEquals "10" (Num 10)
            , test "parses floats" <|
                \_ ->
                    parseEquals "10.5" (Num 10.5)
            , test "parses negative numbers" <|
                \_ ->
                    parseEquals "-10.0" (Num -10)
            ]
        , describe "simple math"
            [ test "addition" <|
                \_ ->
                    parseEquals "10 + 20" (Add (Num 10) (Num 20))
            , test "subtraction" <|
                \_ ->
                    parseEquals "10 - 5" (Sub (Num 10) (Num 5))
            , test "multiplication" <|
                \_ ->
                    parseEquals "10 * 20.0" (Mul (Num 10) (Num 20))
            , test "division" <|
                \_ ->
                    parseEquals "10 / 2" (Div (Num 10) (Num 2))
            , test "exponents" <|
                \_ ->
                    parseEquals "10 ^ 2" (Exp (Num 10) (Num 2))
            , test "parenthesis" <|
                \_ ->
                    parseEquals "(1 + 2) * 3"
                        (Mul
                            (Add (Num 1) (Num 2))
                            (Num 3)
                        )
            ]
        , describe "ordering"
            [ test "add and sub" <|
                \_ ->
                    parseEquals "1 + 2 - 3"
                        (Sub
                            (Add (Num 1) (Num 2))
                            (Num 3)
                        )
            , test "sub and add" <|
                \_ ->
                    parseEquals "1 - 2 + 3"
                        (Add
                            (Sub (Num 1) (Num 2))
                            (Num 3)
                        )
            , test "add and mul" <|
                \_ ->
                    parseEquals "1 + 2 * 3"
                        (Add
                            (Num 1)
                            (Mul (Num 2) (Num 3))
                        )
            , test "two muls and one add" <|
                \_ ->
                    parseEquals "1 * 2 + 3 * 4"
                        (Add
                            (Mul (Num 1) (Num 2))
                            (Mul (Num 3) (Num 4))
                        )
            , test "add and div" <|
                \_ ->
                    parseEquals "1 + 2 / 3"
                        (Add
                            (Num 1)
                            (Div (Num 2) (Num 3))
                        )
            , test "mul and div" <|
                \_ ->
                    parseEquals "1 / 2 * 3"
                        (Mul
                            (Div (Num 1) (Num 2))
                            (Num 3)
                        )
            , test "PEMDAS" <|
                \_ ->
                    parseEquals
                        "(1 + 2) ^ 3 * 4 / 5 + 6 - 7"
                        (Sub
                            (Add
                                (Div
                                    (Mul
                                        (Exp
                                            (Add (Num 1) (Num 2))
                                            (Num 3)
                                        )
                                        (Num 4)
                                    )
                                    (Num 5)
                                )
                                (Num 6)
                            )
                            (Num 7)
                        )
            ]
        ]


parseEquals : String -> Expression -> Expect.Expectation
parseEquals str value =
    str
        |> Parser.run Expression.parser
        |> Expect.equal (Ok value)
