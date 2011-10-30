module JSRefactor.JavaScript.Parser
    (
      innerstring
    ) where

import JSRefactor.ParseLib

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
