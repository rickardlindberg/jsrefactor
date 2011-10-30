module JSRefactor.JavaScript.Printer
    (
      printInnerString
    ) where

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
