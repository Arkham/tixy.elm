module ExpressionSpec exposing (spec)

import Expect
import Expression
import Parser
import Test exposing (..)



-- TESTS


spec : Test
spec =
    describe "Expression"
        [ test "it does stuff" <|
            \_ ->
                "foobar"
                    |> Parser.run Expression.parser
                    |> Expect.equal (Ok Expression.Value)
        ]
