module JSRefactor.JS.Printer
    (
      printValue
    , printInnerString
    ) where

import JSRefactor.JS.Types
import Data.List (intercalate)

printValue :: Value -> String

printValue (Value statements whitespace) =
    (intercalate "" (map printStatement statements)) ++ whitespace

printStatement (DisruptiveStatement s disruptive) =
    s ++ (printDisruptiveStatement disruptive)

printDisruptiveStatement (BreakStatement s1 label s2) =
    "break" ++ s1 ++ label ++ s2 ++ ";"

printDisruptiveStatement (EmptyBreakStatement s) =
    "break" ++ s ++ ";"

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
