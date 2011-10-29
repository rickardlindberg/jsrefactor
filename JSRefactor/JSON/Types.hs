module JSRefactor.JSON.Types
    (
      Value(..)
    , Pair(..)
    ) where

data Value = String WhiteSpace String         WhiteSpace
           | Number WhiteSpace String         WhiteSpace
           | Array  WhiteSpace String [Value] WhiteSpace
           | Object WhiteSpace String [Pair ] WhiteSpace
           deriving (Eq, Show)

data Pair  = Pair (WhiteSpace, Value, WhiteSpace) Value
           deriving (Eq, Show)

type WhiteSpace = String
