module JSRefactor.JS.Parser
    (
      parseJSFile
    , innerstring
    ) where

import JSRefactor.JS.Types
import JSRefactor.ParseLib

parseJSFile :: String -> Either String Value
parseJSFile input =
    case jsFile (initialParseState input) of
        Left  errorMessage -> Left  errorMessage
        Right (value, _)   -> Right value

jsFile        =  (many statement)
             <&> whitespace
             <&> eof
             ==> (\((vs, s), _) -> Value vs s)

statement     =  whitespace
             <&> dStatement
             ==> (\(s, ds) -> DisruptiveStatement s ds)

dStatement    =  breakSt

breakSt       =  (terminal "break")
             <&> (breakStLabel <|> breakStEmpty)
             ==> (\(_, v) -> v)

breakStLabel  =  (atLeastOnce whitespaceC)
             <&> name
             <&> whitespace
             <&> (terminal ";")
             ==> (\(((s1, v), s2), _) -> BreakStatement s1 v s2)

breakStEmpty  =  whitespace
             <&> (terminal ";")
             ==> (\(s, _) -> EmptyBreakStatement s)

name          =  (atLeastOnce (oneCharOf "abc"))

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

whitespace    =  many whitespaceC

whitespaceC   =  (oneCharOf " \n")
