module JSRefactor.JSON.Types
    (
      JValue
    , PureJValue(..)
    ) where

type JValue = (String, PureJValue, String)

data PureJValue = JString String
                | JNumber String
                | JList String [JValue]
                | JObject String [JPair]

type JPair = (JKey, JValue)

type JKey = (String, PureJValue, String)
