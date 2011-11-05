module JSRefactor.JS.Types
    (
      Value(..)
    ) where

data Value = Statements  WhiteSpace
           deriving (Eq, Show)

type WhiteSpace = String
