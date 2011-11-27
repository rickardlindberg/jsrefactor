module JSRefactor.JS.Printer
    (
      printValue
    , printInnerString
    ) where

import JSRefactor.JS.Types
import Data.List (intercalate)

printValue :: Value -> String

printValue (Value statements whitespace) =
    (intercalate "" (map pStmt statements)) ++ whitespace

pStmt (DisruptiveStatement s disruptive) = s ++ (pDisruptiveStmt disruptive)

pDisruptiveStmt (BreakStatement b)   = pBreakStmt b
pDisruptiveStmt (ReturnStatement r)  = pReturnStmt r
pDisruptiveStmt (ThrowStatement s e) = "throw" ++ s ++ (printExpression e) ++ ";"

pBreakStmt (LabeledBreadStatement s1 label s2) = "break" ++ s1 ++ label ++ s2 ++ ";"
pBreakStmt (EmptyBreakStatement s)             = "break" ++ s ++ ";"

pReturnStmt (ExpressionReturnStatement s e) = "return" ++ s ++ (printExpression e) ++ ";"
pReturnStmt (EmptyReturnStatement s)        = "return" ++ s ++ ";"

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
