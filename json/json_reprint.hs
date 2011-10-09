import JSRefactor.ParseLib (ParseState(..))
import JSRefactor.JSON.Parser (pJValue)
import JSRefactor.JSON.Printer (printJValue)

main = interact reprint

reprint input =
    case pJValue (ParseState input) of
        Left  msg        -> msg
        Right (value, _) -> printJValue value
