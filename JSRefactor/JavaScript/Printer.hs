module JSRefactor.JavaScript.Printer
    (
      printValue
    , printInnerString
    ) where

import JSRefactor.JavaScript.Types

printValue :: Value -> String

printValue (Statements v) = v

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
