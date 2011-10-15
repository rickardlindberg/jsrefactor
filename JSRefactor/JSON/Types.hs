module JSRefactor.JSON.Types
    (
      WrappedValue(..)
    , Value(..)
    , Pair(..)
    ) where

data WrappedValue = WrappedValue String Value String

data Value = String String
           | Number String
           | Array  String [WrappedValue]
           | Object String [Pair]

data Pair = Pair (String, Value, String) WrappedValue
