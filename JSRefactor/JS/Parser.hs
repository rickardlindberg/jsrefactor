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

jsFile = do
    ss     <- statements
    _      <- eof
    return $  Value ss

statements =
    singleStatement <|> nilStmt

singleStatement = do
    s      <- whitespace
    v      <- statement
    vs     <- statements
    return $  Statement s v vs

nilStmt =
    whitespace ==> EndStatement

statement =
    disruptiveStmt

disruptiveStmt =
    (breakStmt <|> returnStmt <|> throwStmt) ==> DisruptiveStatement

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
    s1     <- reqWhitespace
    e      <- expression
    s2     <- whitespace
    _      <- terminal ";"
    return $  ExpressionReturnStatement s1 e s2

emptyReturnStmt = do
    _      <- terminal "return"
    s      <- whitespace
    _      <- terminal ";"
    return $  EmptyReturnStatement s

throwStmt = do
    _      <- terminal "throw"
    s1     <- reqWhitespace
    e      <- expression
    s2     <- whitespace
    _      <- terminal ";"
    return $  ThrowStatement s1 e s2

expression =
    literalExp

literalExp =
    literal ==> LiteralExpression

literal =
    numberLiteral

numberLiteral = do
    i      <- integer
    f      <- (fraction <|> constant "")
    return $  NumberLiteral (i ++ f)

integer =
    (terminal "0") <|> nonZeroInteger

nonZeroInteger = do
    first  <- nonZeroDigit
    rest   <- (many digit)
    return $  first:rest

fraction = do
    dot    <- (terminal ".")
    digits <- (many digit)
    return $  dot ++ digits

digit            = oneCharOf "1234567890"
nonZeroDigit     = oneCharOf "123456789"

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
