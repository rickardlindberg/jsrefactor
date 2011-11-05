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
        _ -> oneof [ liftM Statements whitespace
                   ]
    where
        whitespace       = listOf (elements " \n")
        newSize          = size `div` 2

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
