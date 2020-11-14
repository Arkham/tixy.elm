module Expression exposing (Expression(..), parser)

import Parser exposing ((|.), (|=), Parser)


type Expression
    = Value


parser : Parser Expression
parser =
    Parser.succeed Value
