module JSRefactor.ParseLib
    (
      initialParseState
    , terminal
    , oneCharOf
    , anyCharBut
    , eof
    , constant
    , optional
    , (<|>)
    , (<&>)
    , (==>)
    , separatedListOf
    , atLeastOnce
    , many
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

oneCharOf :: String -> Parser Char
oneCharOf string (ParseState input) =
    case input of
        ""     -> Left ("Found EOF instead of one of \"" ++ string ++ "\"")
        (x:xs) ->
            case x `elem` string of
                True  -> Right (x, ParseState xs)
                False -> Left  ("Did not find one of \"" ++ string ++ "\"")

anyCharBut :: String -> Parser Char
anyCharBut string (ParseState (x:xs)) =
    case x `elem` string of
        True  -> Left  ("Did not expect character '" ++ [x] ++ "'")
        False -> Right (x, ParseState xs)

eof :: Parser String
eof (ParseState "") = Right ("", (ParseState ""))
eof _               = Left  "Expected EOF"

constant :: a -> Parser a
constant value parseState = Right (value, parseState)

optional :: a -> Parser a -> Parser a
optional defaultValue p state =
    case p state of
        Left _                    -> Right (defaultValue, state)
        Right (product, newState) -> Right (product, newState)

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

atLeastOnce :: Parser a -> Parser [a]
atLeastOnce parser state =
    case many parser state of
        Right ([], _) -> Left "Expected at least one"
        Right (a, b)  -> Right (a, b)

many :: Parser a -> Parser [a]
many parser state =
    case parser state of
        Left  msg            -> Right ([], state)
        Right (v, nextState) ->
            case many parser nextState of
                Right (vs, nextState) -> Right (v:vs, nextState)
