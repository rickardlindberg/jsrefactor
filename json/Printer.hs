module Printer
    (
      printJValue
    ) where

import Data.List (intercalate)
import JSONParser

printJValue :: JValue -> String
printJValue (spaceBefore, pureJValue, spaceAfter) =
    spaceBefore ++ (printPureJValue pureJValue) ++ spaceAfter
        where printPureJValue (JString x) = "\"" ++ x ++ "\""
              printPureJValue (JNumber x) = x
              printPureJValue (JList   space x) = "[" ++ space ++ (intercalate "," (map printJValue x)) ++ "]"
