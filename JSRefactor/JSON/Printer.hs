module JSRefactor.JSON.Printer
    (
      printValue
    ) where

import Data.List (intercalate)
import JSRefactor.JavaScript.Printer (printInnerString)
import JSRefactor.JSON.Parser
import JSRefactor.JSON.Types

printValue :: Value -> String

printValue (String s1 v s2) =
    s1 ++ (printString v) ++ s2

printValue (Number s1 v s2) =
    s1 ++ v ++ s2

printValue (Boolean s1 True s2) =
    s1 ++ "true" ++ s2

printValue (Boolean s1 False s2) =
    s1 ++ "false" ++ s2

printValue (Null s1 s2) =
    s1 ++ "null" ++ s2

printValue (Array s1 v s2) =
    s1 ++ "[" ++ (printInnerArray v) ++ "]" ++ s2
    where
        printInnerArray (Left  s) = s
        printInnerArray (Right v) = (intercalate "," (map printValue v))

printValue (Object s1 v s2) =
    s1 ++ "{" ++ (printInnerObject v) ++ "}" ++ s2
    where
        printInnerObject (Left  s) = s
        printInnerObject (Right v) = (intercalate "," (map printPair v))

printPair (Pair s1 k s2 v) =
    s1 ++ (printString k) ++ s2 ++ ":" ++ (printValue v)

printString v =
    "\"" ++ (printInnerString v) ++ "\""
