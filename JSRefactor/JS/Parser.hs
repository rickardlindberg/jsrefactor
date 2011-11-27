module JSRefactor.JS.Parser
    (
      parseJSFile
    , innerString
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
    numberLiteral <|> stringLiteral

numberLiteral = do
    i      <- integer
    f      <- (fraction <|> constant "")
    e      <- (exponent_ <|> constant "")
    return $  NumberLiteral (i ++ f ++ e)

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

exponent_ = do
    e      <- (terminal "e" <|> terminal "E")
    p      <- (terminal "+" <|> terminal "-" <|> terminal "")
    d      <- (atLeastOnce digit)
    return $  e ++ p ++ d

stringLiteral =
    (doubleQuotedString <|> singleQuotedString) ==> StringLiteral

doubleQuotedString = do
    _      <- (terminal "\"")
    s      <- (innerString '"')
    _      <- (terminal "\"")
    return $  DoubleQuotedString s

singleQuotedString = do
    _      <- (terminal "'")
    s      <- (innerString '\'')
    _      <- (terminal "'")
    return $  SingleQuotedString s

digit            = oneCharOf "1234567890"
nonZeroDigit     = oneCharOf "123456789"

name             =  atLeastOnce $ oneCharOf $ ['a'..'z'] ++ ['A'..'Z']

innerString quoteChar = many (escaped <|> unescaped)
    where
        escaped    = do
            _      <- (terminal "\\")
            c      <- (oneCharOf $ quoteChar:"\\/bfnrt")
            return $  unescape c
        unescape c | c == quoteChar =  quoteChar
                   | c == '\\'      =  '\\'
                   | c == '/'       =  '/'
                   | c == 'b'       =  '\b'
                   | c == 'f'       =  '\f'
                   | c == 'n'       =  '\n'
                   | c == 'r'       =  '\r'
                   | c == 't'       =  '\t'
        unescaped  =  anyCharBut [quoteChar, '\\', '\b', '\f', '\n', '\r', '\t']

whitespace       =  many        oneWhitespace
reqWhitespace    =  atLeastOnce oneWhitespace
oneWhitespace    =  oneCharOf [' ', '\n']
