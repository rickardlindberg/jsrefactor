module JSRefactor.JSON.Types
    (
      Value(..)
    , Pair(..)
    ) where

data Value = String  WhiteSpace String                      WhiteSpace
           | Number  WhiteSpace Number                      WhiteSpace
           | Boolean WhiteSpace Bool                        WhiteSpace
           | Array   WhiteSpace (Either WhiteSpace [Value]) WhiteSpace
           | Object  WhiteSpace (Either WhiteSpace [Pair ]) WhiteSpace
           deriving (Eq, Show)

data Pair  = Pair WhiteSpace String WhiteSpace Value
           deriving (Eq, Show)

type WhiteSpace = String
type Number = String
