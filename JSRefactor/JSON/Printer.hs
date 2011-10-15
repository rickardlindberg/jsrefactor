module JSRefactor.JSON.Printer
    (
      printJValue
    ) where

import Data.List (intercalate)
import JSRefactor.JSON.Parser
import JSRefactor.JSON.Types

printJValue :: JValue -> String

printJValue (spaceBefore, pureJValue, spaceAfter) =
    spaceBefore ++ (printPureJValue pureJValue) ++ spaceAfter

printPureJValue (JString string) =
    "\"" ++ string ++ "\""
printPureJValue (JNumber number) =
    number
printPureJValue (JList space values) =
    "[" ++ space ++ (intercalate "," (map printJValue values)) ++ "]"
printPureJValue (JObject space pairs) =
    "{" ++ space ++ (intercalate "," (map printPair pairs)) ++ "}"

printPair ((s1, pure, s2), val) =
    s1 ++ (printPureJValue pure) ++ s2 ++ ":" ++ (printJValue val)
