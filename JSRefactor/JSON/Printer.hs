module JSRefactor.JSON.Printer
    (
      printWrappedValue
    ) where

import Data.List (intercalate)
import JSRefactor.JSON.Parser
import JSRefactor.JSON.Types

printWrappedValue :: WrappedValue -> String

printWrappedValue (WrappedValue spaceBefore pureJValue spaceAfter) =
    spaceBefore ++ (printValue pureJValue) ++ spaceAfter

printValue (String string) =
    "\"" ++ string ++ "\""
printValue (Number number) =
    number
printValue (Array space values) =
    "[" ++ space ++ (intercalate "," (map printWrappedValue values)) ++ "]"
printValue (Object space pairs) =
    "{" ++ space ++ (intercalate "," (map printPair pairs)) ++ "}"

printPair (Pair (s1, pure, s2) val) =
    s1 ++ (printValue pure) ++ s2 ++ ":" ++ (printWrappedValue val)
