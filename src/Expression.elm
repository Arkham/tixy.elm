module Expression exposing (Expression(..), Variable(..), parser)

import Parser exposing (..)


type Expression
    = Num Float
    | Var Variable
    | Add Expression Expression
    | Sub Expression Expression
    | Mul Expression Expression
    | Div Expression Expression
    | Exp Expression Expression
    | Sin Expression
    | Cos Expression
    | Tan Expression
    | Asin Expression
    | Acos Expression
    | Atan Expression
    | Abs Expression
    | Sqrt Expression


type Variable
    = T
    | I
    | X
    | Y


parser : Parser Expression
parser =
    expression


digits : Parser Expression
digits =
    let
        numParser =
            number
                { int = Just toFloat
                , hex = Nothing
                , octal = Nothing
                , binary = Nothing
                , float = Just identity
                }
    in
    Parser.map Num <|
        oneOf
            [ succeed negate
                |. symbol "-"
                |= numParser
            , numParser
            ]


variable : Parser Expression
variable =
    Parser.succeed Var
        |= oneOf
            [ Parser.succeed T |. keyword "t"
            , Parser.succeed I |. keyword "i"
            , Parser.succeed X |. keyword "x"
            , Parser.succeed Y |. keyword "y"
            ]


mathFunctions : Parser (Expression -> Expression)
mathFunctions =
    oneOf
        [ Parser.succeed Sin |. keyword "sin"
        , Parser.succeed Cos |. keyword "cos"
        , Parser.succeed Tan |. keyword "tan"
        , Parser.succeed Asin |. keyword "asin"
        , Parser.succeed Acos |. keyword "acos"
        , Parser.succeed Atan |. keyword "atan"
        , Parser.succeed Abs |. keyword "abs"
        , Parser.succeed Sqrt |. keyword "sqrt"
        ]


{-| A term is a standalone chunk of math, like `4` or `(3 + 4)`. We use it as
a building block in larger expressions.
-}
term : Parser Expression
term =
    oneOf
        [ digits
        , variable
        , succeed identity
            |. symbol "("
            |. spaces
            |= lazy (\_ -> expression)
            |. spaces
            |. symbol ")"
        , succeed (\fn value -> fn value)
            |. spaces
            |= mathFunctions
            |. spaces
            |= lazy (\_ -> expression)
        ]


{-| Every expression starts with a term. After that, it may be done, or there
may be more math.
-}
expression : Parser Expression
expression =
    term
        |> andThen (expressionHelp [])


{-| Once you have parsed a term, you can start looking for more operators.
I am tracking everything as a list, that way I can be sure to follow the order
of operations (PEMDAS) when building the final expression.
In one case, I need an operator and another term. If that happens I keep
looking for more. In the other case, I am done parsing, and I finalize the
expression.
-}
expressionHelp : List ( Expression, Operator ) -> Expression -> Parser Expression
expressionHelp revOps expr =
    oneOf
        [ succeed Tuple.pair
            |. spaces
            |= operator
            |. spaces
            |= term
            |> andThen
                (\( op, newExpr ) ->
                    expressionHelp (( expr, op ) :: revOps) newExpr
                )
        , lazy (\_ -> succeed (finalize revOps expr))
        ]


type Operator
    = AddOp
    | SubOp
    | MulOp
    | DivOp
    | ExpOp


type Associativity
    = LeftAssociative
    | RightAssociative


operator : Parser Operator
operator =
    oneOf
        [ map (\_ -> AddOp) (symbol "+")
        , map (\_ -> SubOp) (symbol "-")
        , map (\_ -> MulOp) (symbol "*")
        , map (\_ -> DivOp) (symbol "/")
        , map (\_ -> ExpOp) (symbol "^")
        ]


precedence : Operator -> Int
precedence op =
    case op of
        AddOp ->
            0

        SubOp ->
            0

        MulOp ->
            1

        DivOp ->
            1

        ExpOp ->
            2


opToExpr : Operator -> (Expression -> Expression -> Expression)
opToExpr op =
    case op of
        AddOp ->
            Add

        SubOp ->
            Sub

        MulOp ->
            Mul

        DivOp ->
            Div

        ExpOp ->
            Exp


associativity : Operator -> Associativity
associativity op =
    case op of
        AddOp ->
            LeftAssociative

        SubOp ->
            LeftAssociative

        MulOp ->
            LeftAssociative

        DivOp ->
            LeftAssociative

        ExpOp ->
            RightAssociative


{-| This function is using the shunting yard algorithm.

Imagine we have the expression `1 + 2 * 3`. We'd like to reduce it like this:

> finalize [ ( Num 2, MulOp ), ( Num 1, AddOp ) ] (Num 3)
> finalize [ ( Num 1, AddOp ) ] (Mul (Num 2) (Num 3))
> finalize [] (Add (Num 1) (Mul (Num 2) (Num 3)))
> Add (Num 1) (Mul (Num 2) (Num 3))

If instead we have the expression `1 * 2 + 3`:

> finalize [ ( Num 2, AddOp ), ( Num 1, MulOp ) ] (Num 3)
> Add (finalize [ ( Num 1, MulOp ) ] (Num 2)) (Num 3)
> Add (finalize [] (Mul (Num 1) (Num 2))) (Num 3)
> Add (Mul (Num 1) (Num 2)) (Num 3)

-}
finalize : List ( Expression, Operator ) -> Expression -> Expression
finalize revOps finalExpr =
    case revOps of
        [] ->
            finalExpr

        ( firstExpr, firstOp ) :: otherRevOps ->
            let
                expr =
                    opToExpr firstOp

                assoc =
                    associativity firstOp

                anyOtherHasLowerPrecedence =
                    case associativity firstOp of
                        LeftAssociative ->
                            List.any
                                (\( _, op ) -> precedence firstOp > precedence op)
                                otherRevOps

                        RightAssociative ->
                            List.any
                                (\( _, op ) -> precedence firstOp >= precedence op)
                                otherRevOps
            in
            if anyOtherHasLowerPrecedence then
                finalize otherRevOps (expr firstExpr finalExpr)

            else
                expr (finalize otherRevOps firstExpr) finalExpr