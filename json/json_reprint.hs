import Data.List (intercalate, stripPrefix)

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

-- PARSE LIBRARY

type Parser a = ParseState -> (Either ErrorMessage (a, ParseState))
data ParseState = ParseState {
    input :: String
}
type ErrorMessage = String

expect :: String -> Parser String
expect string (ParseState input) =
    case string `stripPrefix` input of
        Nothing        -> Left "Did not find expected string"
        Just restInput -> Right (string, ParseState restInput)

eatAtLeastOneChars :: [Char] -> Parser String
eatAtLeastOneChars chars state =
    case eatChars chars state of
        Right ("", state) -> Left("Expected at least one char")
        foo               -> foo

eatChars :: [Char] -> Parser String
eatChars chars (ParseState input) = Right(parsed, ParseState rest)
    where parsed = takeWhile (\s -> s `elem` chars) input
          rest   = drop (length parsed) input

pOr :: Parser a -> Parser a -> Parser a
pOr first second state =
    case first state of
        Left (a)     -> second state
        Right (a, b) -> Right (a, b)

pAnd :: Parser a -> Parser b -> Parser (a, b)
pAnd first second state =
    case first state of
        Left (msg)               -> Left (msg)
        Right (aValue, newState) ->
            case second newState of
                Left (msg)               -> Left (msg)
                Right (bValue, newState) -> Right ((aValue, bValue), newState)

pConvert :: Parser a -> (a -> b) -> Parser b
pConvert parser predicate state =
    case parser state of
        Left (msg)          -> Left (msg)
        Right (a, newState) -> Right(predicate a, newState)

pList :: String -> Parser a -> Parser [a]
pList separator itemParser state =
    case itemParser state of
        Left _                  -> Right ([], state)
        Right (item, nextState) ->
            case pRestList separator itemParser nextState of
                Left msg -> Left msg
                Right (items, nextState) -> Right (item:items, nextState)

pRestList :: String -> Parser a -> Parser [a]
pRestList separator itemParser =
    pWhile (expect separator `pAnd` itemParser `pConvert` foo)
        where foo (a, b) = b

pWhile :: Parser a -> Parser [a]
pWhile parser state =
    case parser state of
        Left msg             -> Right ([], state)
        Right (a, nextState) ->
            case pWhile parser nextState of
                Right (list, nextState) -> Right (a:list, nextState)
