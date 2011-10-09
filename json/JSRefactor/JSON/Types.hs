module JSRefactor.JSON.Types
    (
      JValue
    , Space
    , PureJValue(..)
    ) where

type JValue = (Space, PureJValue, Space)

type Space = String

data PureJValue = JString String
                | JNumber String
                | JList Space [JValue]

