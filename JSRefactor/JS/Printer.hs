module JSRefactor.JS.Printer
    (
      printValue
    , printInnerString
    ) where

import JSRefactor.JS.Types
import Data.List (intercalate)

printValue :: Value -> String
printValue (Value s) = pStmts s

pStmts (Statement s1 s ss) = s1 ++ (pStmt s) ++ (pStmts ss)
pStmts (EndStatement s)    = s

pStmt (DisruptiveStatement disruptive) = pDisruptiveStmt disruptive

pDisruptiveStmt (BreakStatement b)       = pBreakStmt b
pDisruptiveStmt (ReturnStatement r)      = pReturnStmt r
pDisruptiveStmt (ThrowStatement s1 e s2) = "throw" ++ s1 ++ (pExp e) ++ s2 ++ ";"

pBreakStmt (LabeledBreadStatement s1 label s2) = "break" ++ s1 ++ label ++ s2 ++ ";"
pBreakStmt (EmptyBreakStatement s)             = "break" ++ s ++ ";"

pReturnStmt (ExpressionReturnStatement s1 e s2) = "return" ++ s1 ++ (pExp e) ++ s2 ++ ";"
pReturnStmt (EmptyReturnStatement s)            = "return" ++ s ++ ";"

pExp (LiteralExpression s) = pLiteralExp s

pLiteralExp (NumberLiteral s) = s
pLiteralExp (StringLiteral s) = pStringLiteral s

pStringLiteral (DoubleQuotedString s) = "\"" ++ (printInnerString '"' s) ++ "\""
pStringLiteral (SingleQuotedString s) = "'" ++ (printInnerString '\'' s) ++ "'"

printInnerString :: Char -> String -> String
printInnerString quoteChar s = concatMap escapeChar s
    where
        escapeChar c | c == quoteChar = "\\" ++ [quoteChar]
                     | c == '\\'      = "\\\\"
                     | c == '/'       = "\\/"
                     | c == '\b'      = "\\b"
                     | c == '\f'      = "\\f"
                     | c == '\n'      = "\\n"
                     | c == '\r'      = "\\r"
                     | c == '\t'      = "\\t"
                     | otherwise      = [c]
