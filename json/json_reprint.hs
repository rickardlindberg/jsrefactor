import ParseLib
import JSONParser
import Printer

main = interact reprint

reprint input =
    case pJValue (ParseState input) of
        Left  msg        -> msg
        Right (value, _) -> printJValue value
