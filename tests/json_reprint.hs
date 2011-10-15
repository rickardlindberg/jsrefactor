import JSRefactor.ParseLib (ParseState(..))
import JSRefactor.JSON.Parser (parseJSONFile)
import JSRefactor.JSON.Printer (printJValue)

main = interact reprint

reprint input =
    case parseJSONFile (ParseState input) of
        Left  msg        -> msg
        Right (value, _) -> printJValue value
