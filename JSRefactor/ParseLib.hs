module JSRefactor.ParseLib
    (
      runParser
    , initialParseState
    , constant
    , terminal
    , oneCharOf
    , anyCharBut
    , eof
    , (<|>)
    , (<&>)
    , (==>)
    , separatedListOf
    , atLeastOnce
    , many
    ) where

import Data.List (stripPrefix)

newtype Parser a = Parser {
    runParser :: ParseState -> Either String (a, ParseState)
}

data ParseState = ParseState {
    input :: String
}

initialParseState input = ParseState input

constant :: a -> Parser a
constant value = Parser $ \parseState -> Right (value, parseState)

terminal :: String -> Parser String
terminal string = Parser $ \(ParseState input) ->
    case string `stripPrefix` input of
        Nothing        -> Left  ("Did not find expected string \"" ++ string ++ "\"")
        Just restInput -> Right (string, ParseState restInput)

oneCharOf :: String -> Parser Char
oneCharOf string = Parser $ \(ParseState input) ->
    case input of
        ""     -> Left ("Found EOF instead of one of \"" ++ string ++ "\"")
        (x:xs) ->
            case x `elem` string of
                True  -> Right (x, ParseState xs)
                False -> Left  ("Did not find one of \"" ++ string ++ "\"")

anyCharBut :: String -> Parser Char
anyCharBut string = Parser $ \(ParseState (x:xs)) ->
    case x `elem` string of
        True  -> Left  ("Did not expect character '" ++ [x] ++ "'")
        False -> Right (x, ParseState xs)

eof :: Parser String
eof = Parser $ \state ->
          case state of
             (ParseState "") -> Right ("", (ParseState ""))
             _               -> Left  "Expected EOF"

(<|>) :: Parser a -> Parser a -> Parser a
(Parser first) <|> (Parser second) = Parser $ \state ->
    case first state of
        Left  (a)    ->
            case second state of
                Left  (a')   -> Left (a ++ "\n" ++ a')
                Right (a, b) -> Right (a, b)
        Right (a, b) -> Right (a, b)

(<&>) :: Parser a -> Parser b -> Parser (a, b)
(Parser first) <&> (Parser second) = Parser $ \state ->
    case first state of
        Left (msg)               -> Left (msg)
        Right (aValue, newState) ->
            case second newState of
                Left (msg)               -> Left (msg)
                Right (bValue, newState) -> Right ((aValue, bValue), newState)

(==>) :: Parser a -> (a -> b) -> Parser b
(Parser parser) ==> predicate = Parser $ \state ->
    case parser state of
        Left (msg)          -> Left (msg)
        Right (a, newState) -> Right(predicate a, newState)

separatedListOf :: String -> Parser a -> Parser [a]
separatedListOf separator (Parser itemParser) = Parser $ \state ->
    case itemParser state of
        Left _                  -> Left ("No items in list")
        Right (item, nextState) ->
             case runParser (pRestList separator (Parser itemParser)) nextState of
                Left msg -> Left msg
                Right (items, nextState) -> Right (item:items, nextState)

pRestList :: String -> Parser a -> Parser [a]
pRestList separator itemParser =
    many (terminal separator <&> itemParser ==> foo)
        where foo (a, b) = b

atLeastOnce :: Parser a -> Parser [a]
atLeastOnce parser = Parser $ \state ->
    case runParser (many parser) state of
        Right ([], _) -> Left "Expected at least one"
        Right (a, b)  -> Right (a, b)

many :: Parser a -> Parser [a]
many parser = Parser $ \state ->
    case runParser parser state of
        Left  msg            -> Right ([], state)
        Right (v, nextState) ->
            case runParser (many parser) nextState of
                Right (vs, nextState) -> Right (v:vs, nextState)

instance Monad Parser where
    return = constant
    first >>= f  = Parser $ \state ->
        case runParser first state of
            Left msg                  -> Left msg
            Right (product, newState) -> runParser (f product) newState
