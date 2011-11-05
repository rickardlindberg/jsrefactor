module JSRefactor.JavaScript.Parser
    (
      parseJSFile
    , innerstring
    ) where

import JSRefactor.JavaScript.Types
import JSRefactor.ParseLib

parseJSFile :: String -> Either String Value
parseJSFile input =
    case jsFile (initialParseState input) of
        Left  errorMessage -> Left  errorMessage
        Right (value, _)   -> Right value

jsFile        =  whitespace
             <&> eof
             ==> (\(v, _) -> Statements v)

innerstring   =  (many (escaped <|> unescaped))

escaped       =  (terminal "\\") <&> (oneCharOf "\"\\/bfnrt") ==> (\(_, c) -> (unescape c))

unescape '"'  = '"'
unescape '\\' = '\\'
unescape '/'  = '/'
unescape 'b'  = '\b'
unescape 'f'  = '\f'
unescape 'n'  = '\n'
unescape 'r'  = '\r'
unescape 't'  = '\t'

unescaped     =  (anyCharBut ['"', '\\', '\b', '\f', '\n', '\r', '\t'])

whitespace    =  many (oneCharOf " \n")
