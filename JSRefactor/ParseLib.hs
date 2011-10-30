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
    , many
    , anyCharBut
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
        Nothing        -> Left  ("Did not find expected string \"" ++ string ++ "\"")
        Just restInput -> Right (string, ParseState restInput)

anyChar :: Parser Char
anyChar (ParseState "")     = Left "Expected char but found EOF"
anyChar (ParseState (x:xs)) = Right(x, ParseState xs)

anyCharBut :: String -> Parser Char
anyCharBut string (ParseState (x:xs)) =
    case x `elem` string of
        True  -> Left  ("Did not expect character '" ++ [x] ++ "'")
        False -> Right (x, ParseState xs)

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
        Left  (a)    ->
            case second state of
                Left  (a')   -> Left (a ++ "\n" ++ a')
                Right (a, b) -> Right (a, b)
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
        Left _                  -> Left ("No items in list")
        Right (item, nextState) ->
            case pRestList separator itemParser nextState of
                Left msg -> Left msg
                Right (items, nextState) -> Right (item:items, nextState)

pRestList :: String -> Parser a -> Parser [a]
pRestList separator itemParser =
    many (terminal separator <&> itemParser ==> foo)
        where foo (a, b) = b

many :: Parser a -> Parser [a]
many parser state =
    case parser state of
        Left  msg            -> Right ([], state)
        Right (v, nextState) ->
            case many parser nextState of
                Right (vs, nextState) -> Right (v:vs, nextState)
