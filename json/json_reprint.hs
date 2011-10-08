main = interact reprint
    where reprint = printJValue . parseDocument

-- DATA

type JValue = (Space, PureJValue, Space)

type Space = String

data PureJValue = JString String
                | JNumber String

-- PRINT

printJValue :: JValue -> String
printJValue (spaceBefore, pureJValue, spaceAfter) =
    spaceBefore ++ (printPureJValue pureJValue) ++ spaceAfter
        where printPureJValue (JString string) = "\"" ++ string ++ "\""
              printPureJValue (JNumber string) = string

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

parseSpace :: String -> (String, String)
parseSpace "" = ("", "")
parseSpace (x:xs)
    | isSpace x = addToFirst x (parseSpace xs)
    | otherwise = ("", (x:xs))
    where
        isSpace x = x `elem` [' ', '\n']

addToFirst :: a -> ([a], [a]) -> ([a], [a])
addToFirst a (b, c) = (a:b, c)
