import ParseLib (ParseState(..))
import JSONParser (pJValue)
import Printer (printJValue)

main = interact reprint

reprint input =
    case pJValue (ParseState input) of
        Left  msg        -> msg
        Right (value, _) -> printJValue value
