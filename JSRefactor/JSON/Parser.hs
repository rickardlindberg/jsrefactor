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
             <&> barestring
             <&> space
             ==> (\((s1, v), s2) -> String s1 v s2)

number        =  space
             <&> (eatAtLeastOneChars "1234567890")
             <&> space
             ==> (\((s1, v), s2) -> Number s1 v s2)

array         =  space
             <&> (terminal "[")
             <&> ((("," `separatedListOf` value) ==> Right) <|> (space ==> Left))
             <&> (terminal "]")
             <&> space
             ==> (\((((s1, _), v), _), s2) -> Array s1 v s2)

object        =  space
             <&> (terminal "{")
             <&> ((("," `separatedListOf` pair) ==> Right) <|> (space ==> Left))
             <&> (terminal "}")
             <&> space
             ==> (\((((s1, _), v), _), s2) -> Object s1 v s2)

pair          =  space
             <&> barestring
             <&> space
             <&> (terminal ":")
             <&> value
             ==> (\((((s1, k), s2), _), v) -> Pair s1 k s2 v)

barestring    =  (terminal "\"")
             <&> innerstring 
             <&> (terminal "\"")
             ==> (\((_, s), _) -> s)

innerstring   =  (many (escaped <|> unescaped))

escaped       =  (terminal "\\") <&> (oneCharOf "nt") ==> (\(_, c) -> (unescape c))

unescaped     =  (anyCharBut "\\\"")

unescape 'n'  = '\n'
unescape 't'  = '\t'
unescape  _   = error "Stupid programmer"

space         =  eatChars " \n"
