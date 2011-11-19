import JSRefactor.JSON.Parser (parseJSONFile)
import Test.HUnit

canParse :: String -> Bool
canParse input =
    case parseJSONFile input of
        Left _ -> False
        _      -> True

main = runTestTT $ test [
        "parsing invalid input should fail" ~: (canParse "\n33\n\n55") ~=? False
    ]
