module JSRefactor.JSON.Types
    (
      Value(..)
    , Pair(..)
    ) where

data Value = String WhiteSpace String         WhiteSpace
           | Number WhiteSpace String         WhiteSpace
           | Array  WhiteSpace String [Value] WhiteSpace
           | Object WhiteSpace String [Pair ] WhiteSpace

data Pair  = Pair (WhiteSpace, Value, WhiteSpace) Value

type WhiteSpace = String
