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
        where printPureJValue (JString x) = "\"" ++ x ++ "\""
              printPureJValue (JNumber x) = x
              printPureJValue (JList   space x) = "[" ++ space ++ (intercalate "," (map printJValue x)) ++ "]"
              printPureJValue (JObject space x) = "{" ++ space ++ (pairs x) ++ "}"
              pairs x = (intercalate "," (map foo x))
              foo ((s1, pure, s2), val) = s1 ++ (printPureJValue pure) ++ s2 ++ ":" ++ (printJValue val)
