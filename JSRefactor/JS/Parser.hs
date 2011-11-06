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

jsFile           =  many statement
                <&> whitespace
                <&> eof
                ==> (\((vs, s), _) -> Value vs s)

statement        =  disruptiveStmt

disruptiveStmt   =  whitespace
                <&> (breakStmt <|> emptyBreakStmt <|> returnStmt <|> emptyReturnStmt)
                ==> (\(s, ds) -> DisruptiveStatement s ds)

breakStmt        =  terminal "break"
                <&> reqWhitespace
                <&> name
                <&> whitespace
                <&> terminal ";"
                ==> (\((((_, s1), n), s2), _) -> BreakStatement s1 n s2)

emptyBreakStmt   =  terminal "break"
                <&> whitespace
                <&> terminal ";"
                ==> (\((_, s), _) -> EmptyBreakStatement s)

returnStmt       =  terminal "return"
                <&> reqWhitespace
                <&> expression
                <&> terminal ";"
                ==> (\(((_, s1), e), _) -> ReturnStatement s1 e)

expression       =  atLeastOnce $ oneCharOf "1"

emptyReturnStmt  =  terminal "return"
                <&> whitespace
                <&> terminal ";"
                ==> (\((_, s), _) -> EmptyReturnStatement s)

name             =  atLeastOnce $ oneCharOf $ ['a'..'z'] ++ ['A'..'Z']

innerstring      =  many (escaped <|> unescaped)
escaped          =  (terminal "\\") <&> (oneCharOf "\"\\/bfnrt") ==> (\(_, c) -> (unescape c))
unescape '"'     =  '"'
unescape '\\'    =  '\\'
unescape '/'     =  '/'
unescape 'b'     =  '\b'
unescape 'f'     =  '\f'
unescape 'n'     =  '\n'
unescape 'r'     =  '\r'
unescape 't'     =  '\t'
unescaped        =  anyCharBut ['"', '\\', '\b', '\f', '\n', '\r', '\t']

whitespace       =  many        oneWhitespace
reqWhitespace    =  atLeastOnce oneWhitespace
oneWhitespace    =  oneCharOf [' ', '\n']
