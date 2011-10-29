import Control.Monad
import JSRefactor.JSON.Parser
import JSRefactor.JSON.Printer
import JSRefactor.JSON.Types
import Test.QuickCheck

-- Generators

instance Arbitrary Value where
    arbitrary =
        oneof [ liftM3 String whitespace string whitespace
              , liftM3 Number whitespace number whitespace
              , liftM3 Array  whitespace (oneof [liftM Left whitespace, liftM Right (listOf arbitrary)]) whitespace
              , liftM3 Object whitespace (oneof [liftM Left whitespace, liftM Right (listOf arbitrary)]) whitespace
              ]

instance Arbitrary Pair where
    arbitrary =
        liftM4 Pair whitespace string whitespace arbitrary

whitespace = listOf (elements " \n")

number = listOf1 (elements "0123456789")

string = listOf (elements ['a'..'z'])

-- Properties

prop_parsed_and_printed_looks_the_same_as_original value =
    let originalString = printValue value in
        case parseJSONFile originalString of
            Left  _      -> False
            Right value' -> (printValue value') == originalString

-- Runner

main = do
    quickCheckWith (stdArgs { maxSize = 3 }) prop_parsed_and_printed_looks_the_same_as_original
