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
        expression      = return "1"
        whitespace      = listOf  oneWhitespace
        reqWhitespace   = listOf1 oneWhitespace
        oneWhitespace   = elements " \n"
        label           = return "abc"
        newSize         = size `div` 2

-- Properties

prop_parsed_and_printed_is_same_as_original value =
    let originalString = printValue value in
        whenFail (putStr ("\nString: " ++ originalString ++ "Back: " ++ (show (parseJSFile originalString)))) $
        case parseJSFile originalString of
            Left  _      -> False
            Right value' -> (printValue value') == originalString

prop_printed_and_parsed_is_same_as_original value =
    whenFail (putStr ((printValue value) ++ (show (parseJSFile (printValue value))))) $
    case parseJSFile (printValue value) of
        Left  _      -> False
        Right value' -> value' == value

-- Runner

main = do
    quickCheckWith (stdArgs { maxSize = 5 }) prop_parsed_and_printed_is_same_as_original
    quickCheckWith (stdArgs { maxSize = 5 }) prop_printed_and_parsed_is_same_as_original
