import JSRefactor.JSON.Parser (parseJSONFile)

main = interact reprint

reprint input =
    case parseJSONFile input of
        Left  errorMessage -> "FAILURE\n" ++ errorMessage
        Right value        -> "SUCCESS"
