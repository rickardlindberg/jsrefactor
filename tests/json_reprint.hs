import JSRefactor.JSON.Parser (parseJSONFile)
import JSRefactor.JSON.Printer (printWrappedValue)

main = interact reprint

reprint input =
    case parseJSONFile input of
        Left  errorMessage -> errorMessage
        Right value        -> printWrappedValue value
