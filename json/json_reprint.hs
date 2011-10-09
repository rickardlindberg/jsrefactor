import Data.List (intercalate)
import ParseLib

main = interact reprint

reprint input =
    case pJValue (ParseState input) of
        Left  msg        -> msg
        Right (value, _) -> printJValue value

-- DATA

type JValue = (Space, PureJValue, Space)

type Space = String

data PureJValue = JString String
                | JNumber String
                | JList Space [JValue]

-- PRINT

printJValue :: JValue -> String
printJValue (spaceBefore, pureJValue, spaceAfter) =
    spaceBefore ++ (printPureJValue pureJValue) ++ spaceAfter
        where printPureJValue (JString x) = "\"" ++ x ++ "\""
              printPureJValue (JNumber x) = x
              printPureJValue (JList   space x) = "[" ++ space ++ (intercalate "," (map printJValue x)) ++ "]"

-- PARSE

pJValue :: Parser JValue

pJValue = (space `pAnd` pPureJValue `pAnd` space) `pConvert` toJValue
    where toJValue ((s1, p), s2) = (s1, p, s2)

space = eatChars " \n"

pPureJValue = pJString `pOr` pJNumber `pOr` pJList

pJString = (expect "\"") `pAnd`
           (eatAtLeastOneChars (['a'..'z'] ++ ['A'..'Z'] ++ "_ ")) `pAnd`
           (expect "\"") `pConvert`
           toJString
    where toJString ((a, b), c) = JString b

pJNumber = (eatAtLeastOneChars "1234567890") `pConvert` toJNumber
    where toJNumber a = JNumber a

pJList = (expect "[") `pAnd`
         (space) `pAnd`
         (pList "," pJValue) `pAnd`
         (expect "]") `pConvert`
         toJList
    where toJList (((a, b), c), d) = JList b c
