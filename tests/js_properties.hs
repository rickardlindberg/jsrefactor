import Control.Monad
import JSRefactor.JS.Parser
import JSRefactor.JS.Printer
import JSRefactor.JS.Types
import Test.QuickCheck

-- Generators

instance Arbitrary Value where
    arbitrary = sized value

value size = liftM2 Value statements whitespace
    where
        statements      = listOf statement
        statement       = oneof [ liftM2 DisruptiveStatement whitespace dStatement
                                ]
        dStatement      = oneof [ liftM3 BreakStatement       reqWhitespace label whitespace
                                , liftM  EmptyBreakStatement  whitespace
                                , liftM2 ReturnStatement      reqWhitespace expression
                                , liftM  EmptyReturnStatement whitespace
                                ]
        whitespace      = listOf  oneWhitespace
        reqWhitespace   = listOf1 oneWhitespace
        oneWhitespace   = elements " \n"
        expression      = return "1"
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
