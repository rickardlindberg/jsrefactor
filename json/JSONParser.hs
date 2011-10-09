module JSONParser
    (
      JValue
    , pJValue
    , Space
    , PureJValue(..)
    ) where

import ParseLib

type JValue = (Space, PureJValue, Space)

type Space = String

data PureJValue = JString String
                | JNumber String
                | JList Space [JValue]

pJValue :: Parser JValue

pJValue = (space `pAnd` pPureJValue `pAnd` space) `pConvert` toJValue
    where toJValue ((s1, p), s2) = (s1, p, s2)

space = eatChars " \n"

pPureJValue = pJString `pOr` pJNumber `pOr` pJList

pJString = (expect "\"") `pAnd`
           (eatAtLeastOneChars (['a'..'z'] ++ ['A'..'Z'] ++ "_ ")) `pAnd`
           (expect "\"") `pConvert`
           toJString
    where toJString ((a, b), c) = JString b

pJNumber = (eatAtLeastOneChars "1234567890") `pConvert` toJNumber
    where toJNumber a = JNumber a

pJList = (expect "[") `pAnd`
         (space) `pAnd`
         (pList "," pJValue) `pAnd`
         (expect "]") `pConvert`
         toJList
    where toJList (((a, b), c), d) = JList b c
