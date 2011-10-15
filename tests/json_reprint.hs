import JSRefactor.JSON.Parser (parseJSONFile)
import JSRefactor.JSON.Printer (printJValue)

main = interact reprint

reprint input =
    case parseJSONFile input of
        Left  errorMessage -> errorMessage
        Right value        -> printJValue value
