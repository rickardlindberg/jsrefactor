module JSRefactor.JS.Types
    (
      Value(..)
    , Statement(..)
    , DisruptiveStatement(..)
    ) where

data Value = Value [Statement] WhiteSpace
    deriving (Eq, Show)

data Statement =
     DisruptiveStatement WhiteSpace DisruptiveStatement
    deriving (Eq, Show)

data DisruptiveStatement =
      BreakStatement       WhiteSpace String WhiteSpace
    | EmptyBreakStatement  WhiteSpace
    | ReturnStatement      WhiteSpace Expression
    | EmptyReturnStatement WhiteSpace
    | ThrowStatement       WhiteSpace Expression
    deriving (Eq, Show)

type Expression = String

type WhiteSpace = String
