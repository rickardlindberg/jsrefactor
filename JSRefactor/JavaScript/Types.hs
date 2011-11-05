module JSRefactor.JavaScript.Types
    (
      Value(..)
    ) where

data Value = Statements  WhiteSpace
           deriving (Eq, Show)

type WhiteSpace = String
