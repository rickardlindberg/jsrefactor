import Control.Monad
import JSRefactor.JS.Parser
import JSRefactor.JS.Printer
import JSRefactor.JS.Types
import Test.QuickCheck

-- Generators

instance Arbitrary Value where
    arbitrary = sized value

value size = liftM Value stmts
    where
        stmts           = oneof [ liftM3 Statement    whitespace stmt stmts
                                , liftM  EndStatement whitespace
                                ]
        stmt            = oneof [ liftM  DisruptiveStatement disruptiveStmt
                                ]
        disruptiveStmt  = oneof [ liftM  BreakStatement  breakStmt
                                , liftM  ReturnStatement returnStmt
                                , liftM3 ThrowStatement  reqWhitespace expression whitespace
                                ]
        breakStmt       = oneof [ liftM3 LabeledBreadStatement reqWhitespace label whitespace
                                , liftM  EmptyBreakStatement   whitespace
                                ]
        returnStmt      = oneof [ liftM3 ExpressionReturnStatement reqWhitespace expression whitespace
                                , liftM  EmptyReturnStatement      whitespace
                                ]
        expression      = oneof [ liftM  LiteralExpression literal
                                ]
        literal         = oneof [ liftM  NumberLiteral number
                                , liftM  StringLiteral string
                                ]
        number          = integer <++> fraction <++> exponent
        integer         = oneof [ return "0"
                                , liftM2 (:) nonZeroDigit (listOf digit)
                                ]
        fraction        = oneof [ return ""
                                , liftM2 (++) (return ".") (listOf digit)
                                ]
        exponent        = (elements ["e", "E"]) <++> (elements ["", "+", "-"]) <++> (listOf1 digit)
        nonZeroDigit    = elements "123456789"
        digit           = elements "1234567890"
        string          = oneof [ liftM DoubleQuotedString innerString
                                , liftM SingleQuotedString innerString
                                ]
        innerString     = listOf (elements (['"', '\'', '\\', '/', '\b', '\f', '\n', '\r', '\t'] ++ [' '..'ω']))
        whitespace      = listOf  oneWhitespace
        reqWhitespace   = listOf1 oneWhitespace
        oneWhitespace   = elements " \n"
        label           = return "abc"
        newSize         = size `div` 2

(<++>) :: Gen String -> Gen String -> Gen String
first <++> second = liftM2 (++) first second

-- Properties

prop_parsed_and_printed_is_same_as_original value =
    let originalString = printValue value in
        case parseJSFile originalString of
            Left  _      -> False
            Right value' -> (printValue value') == originalString

prop_printed_and_parsed_is_same_as_original value =
    case parseJSFile (printValue value) of
        Left  _      -> False
        Right value' -> value' == value

-- Runner

main = do
    quickCheckWith (stdArgs { maxSize = 5 }) prop_parsed_and_printed_is_same_as_original
    quickCheckWith (stdArgs { maxSize = 5 }) prop_printed_and_parsed_is_same_as_original
