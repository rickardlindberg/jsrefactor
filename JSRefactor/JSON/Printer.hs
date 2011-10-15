module JSRefactor.JSON.Printer
    (
      printValue
    ) where

import Data.List (intercalate)
import JSRefactor.JSON.Parser
import JSRefactor.JSON.Types

printValue :: Value -> String

printValue (String s1 v s2) =
    s1 ++ "\"" ++ v ++ "\"" ++ s2

printValue (Number s1 v s2) =
    s1 ++ v ++ s2

printValue (Array s1 s2 vs s3) =
    s1 ++ "[" ++ s2 ++ (intercalate "," (map printValue vs)) ++ "]" ++ s3

printValue (Object s1 s2 vs s3) =
    s1 ++ "{" ++ s2 ++ (intercalate "," (map printPair vs)) ++ "}" ++ s3

printPair (Pair (s1, k, s2) v) =
    s1 ++ (printValue k) ++ s2 ++ ":" ++ (printValue v)
