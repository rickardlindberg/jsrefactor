module JSRefactor.JS.Parser
    (
      parseJSFile
    , innerstring
    ) where

import JSRefactor.JS.Types
import JSRefactor.ParseLib

parseJSFile :: String -> Either String Value
parseJSFile input =
    case runParser jsFile (initialParseState input) of
        Left  errorMessage -> Left  errorMessage
        Right (value, _)   -> Right value

jsFile           =  many statement
                <&> whitespace
                <&> eof
                ==> (\((vs, s), _) -> Value vs s)

statement        =  disruptiveStmt

disruptiveStmt = do
    s      <- whitespace
    d      <- (breakStmt <|> returnStmt <|> throwStmt)
    return $  DisruptiveStatement s d

breakStmt =
    (labeledBreakStmt <|> emptyBreakStmt) ==> BreakStatement

labeledBreakStmt = do
    _      <- (terminal "break")
    s1     <- reqWhitespace
    n      <- name
    s2     <- whitespace
    _      <- (terminal ";")
    return $  LabeledBreadStatement s1 n s2

emptyBreakStmt = do
    _      <- (terminal "break")
    s      <- whitespace
    _      <- (terminal ";")
    return $  EmptyBreakStatement s

returnStmt =
    (exprReturnStmt <|> emptyReturnStmt) ==> ReturnStatement

exprReturnStmt = do
    _      <- terminal "return"
    s      <- reqWhitespace
    e      <- expression
    _      <- terminal ";"
    return $  ExpressionReturnStatement s e

emptyReturnStmt = do
    _      <- terminal "return"
    s      <- whitespace
    _      <- terminal ";"
    return $  EmptyReturnStatement s

throwStmt        =  terminal "throw"
                <&> reqWhitespace
                <&> expression
                <&> terminal ";"
                ==> (\(((_, s), e), _) -> ThrowStatement s e)

expression       =  atLeastOnce $ oneCharOf "1"

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
