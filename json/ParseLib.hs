module ParseLib
    (
      Parser
    , ParseState(..)
    , ErrorMessage
    , expect
    , eatAtLeastOneChars
    , eatChars
    , pOr
    , pAnd
    , pConvert
    , pList
    ) where

import Data.List (stripPrefix)

type Parser a = ParseState -> (Either ErrorMessage (a, ParseState))
data ParseState = ParseState {
    input :: String
}
type ErrorMessage = String

expect :: String -> Parser String
expect string (ParseState input) =
    case string `stripPrefix` input of
        Nothing        -> Left "Did not find expected string"
        Just restInput -> Right (string, ParseState restInput)

eatAtLeastOneChars :: [Char] -> Parser String
eatAtLeastOneChars chars state =
    case eatChars chars state of
        Right ("", state) -> Left("Expected at least one char")
        foo               -> foo

eatChars :: [Char] -> Parser String
eatChars chars (ParseState input) = Right(parsed, ParseState rest)
    where parsed = takeWhile (\s -> s `elem` chars) input
          rest   = drop (length parsed) input

pOr :: Parser a -> Parser a -> Parser a
pOr first second state =
    case first state of
        Left (a)     -> second state
        Right (a, b) -> Right (a, b)

pAnd :: Parser a -> Parser b -> Parser (a, b)
pAnd first second state =
    case first state of
        Left (msg)               -> Left (msg)
        Right (aValue, newState) ->
            case second newState of
                Left (msg)               -> Left (msg)
                Right (bValue, newState) -> Right ((aValue, bValue), newState)

pConvert :: Parser a -> (a -> b) -> Parser b
pConvert parser predicate state =
    case parser state of
        Left (msg)          -> Left (msg)
        Right (a, newState) -> Right(predicate a, newState)

pList :: String -> Parser a -> Parser [a]
pList separator itemParser state =
    case itemParser state of
        Left _                  -> Right ([], state)
        Right (item, nextState) ->
            case pRestList separator itemParser nextState of
                Left msg -> Left msg
                Right (items, nextState) -> Right (item:items, nextState)

pRestList :: String -> Parser a -> Parser [a]
pRestList separator itemParser =
    pWhile (expect separator `pAnd` itemParser `pConvert` foo)
        where foo (a, b) = b

pWhile :: Parser a -> Parser [a]
pWhile parser state =
    case parser state of
        Left msg             -> Right ([], state)
        Right (a, nextState) ->
            case pWhile parser nextState of
                Right (list, nextState) -> Right (a:list, nextState)
