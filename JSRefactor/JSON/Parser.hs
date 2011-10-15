module JSRefactor.JSON.Parser
    (
      parseJSONFile
    ) where

import JSRefactor.ParseLib
import JSRefactor.JSON.Types

parseJSONFile :: String -> Either ErrorMessage JValue
parseJSONFile input =
    case jsonFile (ParseState input) of
        Left  errorMessage -> Left  errorMessage
        Right (value, _)   -> Right value

jsonFile      =  wrappedValue
             <&> eof
             ==> (\(a, b) -> a)

wrappedValue  =  space
             <&> value
             <&> space
             ==> (\((s1, p), s2) -> (s1, p, s2))

value         =  string
             <|> number
             <|> array

string        =  (terminal "\"")
             <&> (eatAtLeastOneChars (['a'..'z'] ++ ['A'..'Z'] ++ "_ ")) 
             <&> (terminal "\"")
             ==> (\((a, b), c) -> JString b)

number        =  (eatAtLeastOneChars "1234567890")
             ==> JNumber

array         =  (terminal "[")
             <&> (space)
             <&> (pList "," wrappedValue)
             <&> (terminal "]")
             ==> (\(((a, b), c), d) -> JList b c)

space         =  eatChars " \n"
