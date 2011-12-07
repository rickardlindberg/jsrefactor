import Control.Monad
import JSRefactor.JSON.Parser
import JSRefactor.JSON.Printer
import JSRefactor.JSON.Types
import Test.QuickCheck

-- Generators

instance Arbitrary Value where
    arbitrary = sized value

value size =
    case size of
        0 -> oneof [ liftM3 String  whitespace string           whitespace
                   , liftM3 Number  whitespace number           whitespace
                   , liftM3 Boolean whitespace arbitrary        whitespace
                   , liftM2 Null    whitespace                  whitespace
                   , liftM3 Array   whitespace emptyInnerArray  whitespace
                   , liftM3 Object  whitespace emptyInnerObject whitespace
                   ]
        _ -> oneof [ liftM3 String  whitespace string           whitespace
                   , liftM3 Number  whitespace number           whitespace
                   , liftM3 Boolean whitespace arbitrary        whitespace
                   , liftM2 Null    whitespace                  whitespace
                   , liftM3 Array   whitespace innerArray       whitespace
                   , liftM3 Object  whitespace innerObject      whitespace
                   ]
    where
        whitespace       = listOf (elements " \n")
        string           = listOf (elements (['"', '\'', '\\', '/', '\b', '\f', '\n', '\r', '\t'] ++ [' '..'ω']))
        number           = liftM2 (++) sign base
        base             = listOf1 (elements "0123456789")
        sign             = oneof [return "", return "-"]
        emptyInnerArray  = liftM Left whitespace
        emptyInnerObject = liftM Left whitespace
        innerArray       = liftM Right (listOf1 (value newSize))
        innerObject      = liftM Right (listOf1 pair)
        pair             = liftM4 Pair whitespace string whitespace (value newSize)
        newSize          = size `div` 2

-- Properties

prop_parsed_and_printed_is_same_as_original value =
    let originalString = printValue value in
        case parseJSONFile originalString of
            Left  _      -> False
            Right value' -> (printValue value') == originalString

prop_printed_and_parsed_is_same_as_original value =
    case parseJSONFile (printValue value) of
        Left  _      -> False
        Right value' -> value' == value

-- Runner

main = do
    quickCheckWith (stdArgs { maxSize = 5 }) prop_parsed_and_printed_is_same_as_original
    quickCheckWith (stdArgs { maxSize = 5 }) prop_printed_and_parsed_is_same_as_original
