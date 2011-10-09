import Data.List (intercalate, stripPrefix)

main = interact reprint
    where reprint = printJValue . parseDocument

-- DATA

type JValue = (Space, PureJValue, Space)

type Space = String

data PureJValue = JString String
                | JNumber String
                | JList [JValue]

-- PRINT

printJValue :: JValue -> String
printJValue (spaceBefore, pureJValue, spaceAfter) =
    spaceBefore ++ (printPureJValue pureJValue) ++ spaceAfter
        where printPureJValue (JString x) = "\"" ++ x ++ "\""
              printPureJValue (JNumber x) = x
              printPureJValue (JList   x) = "[" ++ (intercalate "," (map printJValue x)) ++ "]"

-- PARSE

parseDocument :: String -> JValue
parseDocument input = jvalue
    where (jvalue, []) = parseJValue input

parseJValue :: String -> (JValue, String)
parseJValue input = let (spaceBefore, restInput1) = parseSpace input
                        (pureJValue , restInput2) = parsePureJValue restInput1
                        (spaceAfter , restInput3) = parseSpace restInput2
                    in ((spaceBefore, pureJValue, spaceAfter), restInput3)
                    where
    parsePureJValue input
        | isStringStart input = let (x, xs) = parseStringValue input in ((JString x), xs)
        | isNumberStart input = let (x, xs) = parseNumberValue input in ((JNumber x), xs)
        | isListStart   input = let (x, xs) = parseList        input in ((JList   x), xs)


isStringStart ('"':xs) = True
isStringStart _        = False

parseStringValue :: String -> (String, String)
parseStringValue ('"':xs) = eatUntilEOS xs
    where eatUntilEOS []       = ([], [])
          eatUntilEOS ('"':xs) = ([], xs)
          eatUntilEOS (x:xs)   = addToFirst x (eatUntilEOS xs)

isNumberStart (x:xs)
    | x `elem` "123456789" = True
    | otherwise            = False

parseNumberValue :: String -> (String, String)
parseNumberValue (x:xs)
    | x `elem` "1234567890" = addToFirst x (parseNumberValue xs)
    | otherwise             = ("", (x:xs))

isListStart (x:xs)
    | x `elem` "[" = True
    | otherwise    = False

parseList :: String -> ([JValue], String)
parseList ('[':restInput) = parseFirstItem restInput

parseFirstItem :: String -> ([JValue], String)
parseFirstItem (']':rest) = ([], rest)
parseFirstItem (rest)     = let (value, restInput) = parseJValue rest
                                (x, xs) = parseItems restInput
                            in (value:x, xs)

parseItems :: String -> ([JValue], String)
parseItems (']':rest) = ([], rest)
parseItems (',':rest) = let (value, restInput) = parseJValue rest
                            (x, xs) = parseItems restInput
                        in (value:x, xs)

parseSpace :: String -> (String, String)
parseSpace "" = ("", "")
parseSpace (x:xs)
    | isSpace x = addToFirst x (parseSpace xs)
    | otherwise = ("", (x:xs))
    where
        isSpace x = x `elem` [' ', '\n']

addToFirst :: a -> ([a], [a]) -> ([a], [a])
addToFirst a (b, c) = (a:b, c)

-- XXXX

pJValue :: Parser JValue

pJValue = (space `pAnd` pPureJValue `pAnd` space) `pConvert` toJValue
    where toJValue ((s1, p), s2) = (s1, p, s2)

space = eatChars " \n"

pPureJValue = pJString `pOr` pJNumber `pOr` pJList

pJString = (expect "\"") `pAnd`
           (eatChars (['a'..'z'] ++ ['A'..'Z'] ++ "_")) `pAnd`
           (expect "\"") `pConvert`
           toJString
    where toJString ((a, b), c) = JString b

pJNumber = (eatChars "1234567890") `pConvert` toJNumber
    where toJNumber a = JNumber a

pJList = (expect "[") `pAnd`
         (pList "," pJValue) `pAnd`
         (expect "]") `pConvert`
         toJList
    where toJList ((a, b), c) = JList b

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
        Left msg                -> Left msg
        Right (item, nextState) -> Right (item:restItems, restState)
            where restItems = []
                  restState = nextState
