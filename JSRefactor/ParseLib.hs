module JSRefactor.ParseLib
    (
      initialParseState
    , terminal
    , eatAtLeastOneChars
    , eatChars
    , eof
    , (<|>)
    , (<&>)
    , (==>)
    , separatedListOf
    ) where

import Data.List (stripPrefix)

type Parser a = ParseState -> (Either String (a, ParseState))

data ParseState = ParseState {
    input :: String
}

initialParseState input = ParseState input

terminal :: String -> Parser String
terminal string (ParseState input) =
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

eof :: Parser String
eof (ParseState "") = Right ("", (ParseState ""))
eof _ = Left "Expected EOF"

(<|>) :: Parser a -> Parser a -> Parser a
(first <|> second) state =
    case first state of
        Left (a)     -> second state
        Right (a, b) -> Right (a, b)

(<&>) :: Parser a -> Parser b -> Parser (a, b)
(first <&> second) state =
    case first state of
        Left (msg)               -> Left (msg)
        Right (aValue, newState) ->
            case second newState of
                Left (msg)               -> Left (msg)
                Right (bValue, newState) -> Right ((aValue, bValue), newState)

(==>) :: Parser a -> (a -> b) -> Parser b
(parser ==> predicate) state =
    case parser state of
        Left (msg)          -> Left (msg)
        Right (a, newState) -> Right(predicate a, newState)

separatedListOf :: String -> Parser a -> Parser [a]
separatedListOf separator itemParser state =
    case itemParser state of
        Left _                  -> Right ([], state)
        Right (item, nextState) ->
            case pRestList separator itemParser nextState of
                Left msg -> Left msg
                Right (items, nextState) -> Right (item:items, nextState)

pRestList :: String -> Parser a -> Parser [a]
pRestList separator itemParser =
    pWhile (terminal separator <&> itemParser ==> foo)
        where foo (a, b) = b

pWhile :: Parser a -> Parser [a]
pWhile parser state =
    case parser state of
        Left msg             -> Right ([], state)
        Right (a, nextState) ->
            case pWhile parser nextState of
                Right (list, nextState) -> Right (a:list, nextState)
