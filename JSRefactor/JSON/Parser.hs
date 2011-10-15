module JSRefactor.JSON.Parser
    (
      parseJSONFile
    ) where

import JSRefactor.JSON.Types
import JSRefactor.ParseLib

parseJSONFile :: String -> Either String Value
parseJSONFile input =
    case jsonFile (initialParseState input) of
        Left  errorMessage -> Left  errorMessage
        Right (value, _)   -> Right value

jsonFile      =  value
             <&> eof
             ==> (\(v, _) -> v)

value         =  string
             <|> number
             <|> array
             <|> object

string        =  space
             <&> (terminal "\"")
             <&> (eatAtLeastOneChars (['a'..'z'] ++ ['A'..'Z'] ++ "_ ")) 
             <&> (terminal "\"")
             <&> space
             ==> (\((((s1, _), v), _), s2) -> String s1 v s2)

number        =  space
             <&> (eatAtLeastOneChars "1234567890")
             <&> space
             ==> (\((s1, v), s2) -> Number s1 v s2)

array         =  space
             <&> (terminal "[")
             <&> space
             <&> ("," `separatedListOf` value)
             <&> (terminal "]")
             <&> space
             ==> (\(((((s1, _), s2), v), _), s3) -> Array s1 s2 v s3)

object        =  space
             <&> (terminal "{")
             <&> space
             <&> ("," `separatedListOf` pair)
             <&> (terminal "}")
             <&> space
             ==> (\(((((s1, _), s2), v), _), s3) -> Object s1 s2 v s3)

pair          =  space
             <&> string
             <&> space
             <&> (terminal ":")
             <&> value
             ==> (\((((s1, k), s2), _), v) -> Pair (s1, k, s2) v)

space         =  eatChars " \n"
