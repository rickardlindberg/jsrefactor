import Control.Monad
import JSRefactor.JS.Parser
import JSRefactor.JS.Printer
import JSRefactor.JS.Types
import Test.QuickCheck

-- Generators

instance Arbitrary Value where
    arbitrary = sized value

value size =
    case size of
        _ -> oneof [ liftM2 Value statements whitespace
                   ]
    where
        statements       = listOf statement
        statement        = oneof [ liftM2 DisruptiveStatement whitespace dStatement
                                 ]
        dStatement       = oneof [ liftM3 BreakStatement whitespaceC (return "abc") whitespace
                                 , liftM  EmptyBreakStatement whitespace
                                 ]
        whitespace       = listOf (elements " \n")
        whitespaceC      = listOf1 (elements " \n")
        newSize          = size `div` 2

-- Properties

prop_parsed_and_printed_is_same_as_original value =
    whenFail (putStr (printValue value)) $
    let originalString = printValue value in
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
