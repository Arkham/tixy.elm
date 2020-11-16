module ExpressionSpec exposing (spec)

import Expect
import Expression exposing (Expression(..), Variable(..))
import Parser
import Test exposing (..)


spec : Test
spec =
    describe "Expression"
        [ parserSpec
        , evaluateSpec
        ]


parserSpec : Test
parserSpec =
    describe "parser"
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
            , test "mod" <|
                \_ ->
                    parseEquals "10 % 2" (Mod (Num 10) (Num 2))
            , test "exponents" <|
                \_ ->
                    parseEquals "10 ^ 2" (Exp (Num 10) (Num 2))
            , test "bitwise AND" <|
                \_ ->
                    parseEquals "10 & 2" (BitwiseAnd (Num 10) (Num 2))
            , test "bitwise Or" <|
                \_ ->
                    parseEquals "10 | 2" (BitwiseOr (Num 10) (Num 2))
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
            , test "add, mul and exp" <|
                \_ ->
                    parseEquals
                        "1 + 2 * 3 * 4 ^ 5"
                        (Add
                            (Num 1)
                            (Mul
                                (Num 2)
                                (Mul
                                    (Num 3)
                                    (Exp (Num 4) (Num 5))
                                )
                            )
                        )
            , test "three exp in a row" <|
                \_ ->
                    parseEquals
                        "4 ^ 3 ^ 2"
                        (Exp
                            (Num 4)
                            (Exp (Num 3) (Num 2))
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
            , test "PEMDAS, reversed" <|
                \_ ->
                    parseEquals
                        "1 - 2 + 3 / 4 * 5 ^ (6 + 4)"
                        (Add
                            (Sub
                                (Num 1)
                                (Num 2)
                            )
                            (Div
                                (Num 3)
                                (Mul
                                    (Num 4)
                                    (Exp (Num 5) (Add (Num 6) (Num 4)))
                                )
                            )
                        )
            , test "more complex example" <|
                \_ ->
                    parseEquals
                        "3 + 4 * 2 / (1 - 5) ^ 2 ^ 3"
                        (Add
                            (Num 3)
                            (Mul
                                (Num 4)
                                (Div
                                    (Num 2)
                                    (Exp
                                        (Sub (Num 1) (Num 5))
                                        (Exp (Num 2) (Num 3))
                                    )
                                )
                            )
                        )
            ]
        , describe "variables"
            [ test "parses t, i, x, y" <|
                \_ ->
                    parseEquals
                        "1 + t - i * x / y"
                        (Sub
                            (Add (Num 1) (Var T))
                            (Mul
                                (Var I)
                                (Div (Var X) (Var Y))
                            )
                        )
            ]
        , describe "math functions"
            [ test "parses sin" <|
                \_ ->
                    parseEquals "sin 30" (Sin (Num 30))
            , test "parses cos" <|
                \_ ->
                    parseEquals "cos 30" (Cos (Num 30))
            , test "parses tan" <|
                \_ ->
                    parseEquals "tan 30" (Tan (Num 30))
            , test "parses asin" <|
                \_ ->
                    parseEquals "asin 30" (Asin (Num 30))
            , test "parses acos" <|
                \_ ->
                    parseEquals "acos 30" (Acos (Num 30))
            , test "parses atan" <|
                \_ ->
                    parseEquals "atan 30" (Atan (Num 30))
            , test "parses abs" <|
                \_ ->
                    parseEquals "abs 30" (Abs (Num 30))
            , test "parses sqrt" <|
                \_ ->
                    parseEquals "sqrt 30" (Sqrt (Num 30))
            , test "more complex example" <|
                \_ ->
                    parseEquals
                        "sin (t - sqrt ((x - 7.5) ^ 2 + (y - 6) ^ 2))"
                        (Sin
                            (Sub
                                (Var T)
                                (Sqrt
                                    (Add
                                        (Exp (Sub (Var X) (Num 7.5)) (Num 2))
                                        (Exp (Sub (Var Y) (Num 6)) (Num 2))
                                    )
                                )
                            )
                        )
            , test "another complex example" <|
                \_ ->
                    parseEquals
                        "sin (2 * atan((y-7.5) / (x-7.5)) + 5 * t)"
                        (Sin
                            (Mul
                                (Num 2)
                                (Atan
                                    (Add
                                        (Div
                                            (Sub (Var Y) (Num 7.5))
                                            (Sub (Var X) (Num 7.5))
                                        )
                                        (Mul (Num 5) (Var T))
                                    )
                                )
                            )
                        )
            ]
        ]


evaluateSpec : Test
evaluateSpec =
    let
        tixy =
            { t = 2
            , i = 3
            , x = 4
            , y = 5
            }
    in
    describe "evaluate"
        [ test "simple number" <|
            \_ ->
                Expression.evaluate tixy (Num 10)
                    |> Expect.equal 10
        , test "simple variable" <|
            \_ ->
                Expression.evaluate tixy (Var T)
                    |> Expect.equal 2
        , describe "simple math"
            [ test "addition" <|
                \_ ->
                    Expression.evaluate tixy (Add (Num 1) (Num 3))
                        |> Expect.equal 4
            , test "subtraction" <|
                \_ ->
                    Expression.evaluate tixy (Sub (Num 1) (Num 3))
                        |> Expect.equal -2
            , test "multiplication" <|
                \_ ->
                    Expression.evaluate tixy (Mul (Num 1) (Num 3))
                        |> Expect.equal 3
            , test "division" <|
                \_ ->
                    Expression.evaluate tixy (Div (Num 4) (Num 2))
                        |> Expect.equal 2
            , test "modulo" <|
                \_ ->
                    Expression.evaluate tixy (Mod (Num 4) (Num 3))
                        |> Expect.equal 1
            , test "negative modulo" <|
                \_ ->
                    Expression.evaluate tixy (Mod (Num -4) (Num 3))
                        |> Expect.equal -1
            , test "exponents" <|
                \_ ->
                    Expression.evaluate tixy (Exp (Num 2) (Num 8))
                        |> Expect.equal 256
            , test "using variables" <|
                \_ ->
                    Expression.evaluate tixy (Add (Var T) (Mul (Var I) (Var X)))
                        |> Expect.equal 14
            ]
        , describe "bitwise operators"
            [ test "bitwise AND" <|
                \_ ->
                    Expression.evaluate tixy (BitwiseAnd (Num 2) (Num 3))
                        |> Expect.equal 2
            , test "bitwise OR" <|
                \_ ->
                    Expression.evaluate tixy (BitwiseOr (Num 3) (Num 5))
                        |> Expect.equal 7
            ]
        , let
            expectEqual v =
                Expect.within (Expect.Absolute 0.0001) v
          in
          describe "math functions"
            [ test "sin" <|
                \_ ->
                    Expression.evaluate tixy (Sin (Num (degrees 30)))
                        |> expectEqual 0.5
            , test "cos" <|
                \_ ->
                    Expression.evaluate tixy (Cos (Num (degrees 60)))
                        |> expectEqual 0.5
            , test "tan" <|
                \_ ->
                    Expression.evaluate tixy (Tan (Num (degrees 45)))
                        |> expectEqual 1
            , test "asin" <|
                \_ ->
                    Expression.evaluate tixy (Asin (Num 0.5))
                        |> expectEqual (degrees 30)
            , test "acos" <|
                \_ ->
                    Expression.evaluate tixy (Acos (Num 0.5))
                        |> expectEqual (degrees 60)
            , test "atan" <|
                \_ ->
                    Expression.evaluate tixy (Atan (Num 1))
                        |> expectEqual (degrees 45)
            , test "abs" <|
                \_ ->
                    Expression.evaluate tixy (Abs (Num -1))
                        |> expectEqual 1
            , test "sqrt" <|
                \_ ->
                    Expression.evaluate tixy (Sqrt (Num 4))
                        |> expectEqual 2
            ]
        ]



-- HELPERS


parseEquals : String -> Expression -> Expect.Expectation
parseEquals str value =
    str
        |> Parser.run Expression.parser
        |> Expect.equal (Ok value)
