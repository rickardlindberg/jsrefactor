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
pDisruptiveStmt (ThrowStatement s1 e s2) = "throw" ++ s1 ++ (printExpression e) ++ s2 ++ ";"

pBreakStmt (LabeledBreadStatement s1 label s2) = "break" ++ s1 ++ label ++ s2 ++ ";"
pBreakStmt (EmptyBreakStatement s)             = "break" ++ s ++ ";"

pReturnStmt (ExpressionReturnStatement s1 e s2) = "return" ++ s1 ++ (printExpression e) ++ s2 ++ ";"
pReturnStmt (EmptyReturnStatement s)            = "return" ++ s ++ ";"

printExpression s = s

printInnerString :: String -> String
printInnerString = concatMap escapeChar
    where
        escapeChar '"'  = "\\\""
        escapeChar '\\' = "\\\\"
        escapeChar '/'  = "\\/"
        escapeChar '\b' = "\\b"
        escapeChar '\f' = "\\f"
        escapeChar '\n' = "\\n"
        escapeChar '\r' = "\\r"
        escapeChar '\t' = "\\t"
        escapeChar c    = [c]
