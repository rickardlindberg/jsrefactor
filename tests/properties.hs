import JSRefactor.JSON.Parser
import JSRefactor.JSON.Printer
import JSRefactor.JSON.Types
import Test.QuickCheck

instance Arbitrary Value where
    arbitrary = elements [(String "" "apa" "")]

prop_roundtrip value =
    case parseJSONFile (printValue value) of
        Left  _      -> False
        Right value' -> value == value'

main = do
    quickCheck prop_roundtrip
