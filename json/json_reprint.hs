main = interact reprint
    where reprint = printJDoc . parseJDoc


type JDocument = (JValue, Whitespace)

data JValue = JString Whitespace String
            | JNumber Whitespace String

type Whitespace = String


parseJDoc :: String -> JDocument
parseJDoc input = (value, whitespace)
    where (value, rest) = parseJValue input
          (whitespace, []) = parseSpace rest

printJDoc :: JDocument -> String
printJDoc (JString spaceBefore string, spaceAfter) =
    spaceBefore ++ "\"" ++ string ++ "\"" ++ spaceAfter
printJDoc (JNumber spaceBefore string, spaceAfter) =
    spaceBefore ++ string ++ spaceAfter


parseJValue :: String -> (JValue, String)
parseJValue input
    | isStringStart restInput = let (x, xs) = parseStringValue restInput in (JString whitespace x, xs)
    | isNumberStart restInput = let (x, xs) = parseNumberValue restInput in (JNumber whitespace x, xs)
    where
        (whitespace, restInput) = parseSpace input

isStringStart ('"':xs) = True
isStringStart _        = False

isNumberStart (x:xs)
    | x `elem` "123456789" = True
    | otherwise            = False

parseSpace :: String -> (Whitespace, String)
parseSpace "" = ("", "")
parseSpace (x:xs)
    | isSpace x = addToFirst x (parseSpace xs)
    | otherwise = ("", (x:xs))
    where
        isSpace x = x `elem` [' ', '\n']

parseStringValue :: String -> (String, String)
parseStringValue ('"':xs) = eatUntilEOS xs
    where eatUntilEOS []       = ([], [])
          eatUntilEOS ('"':xs) = ([], xs)
          eatUntilEOS (x:xs)   = addToFirst x (eatUntilEOS xs)

parseNumberValue :: String -> (String, String)
parseNumberValue (x:xs)
    | x `elem` "1234567890" = addToFirst x (parseNumberValue xs)
    | otherwise             = ("", (x:xs))

addToFirst :: a -> ([a], [a]) -> ([a], [a])
addToFirst a (b, c) = (a:b, c)
