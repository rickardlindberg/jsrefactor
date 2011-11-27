module JSRefactor.JS.Types
    (
      Value(..)
    , Statement(..)
    , DisruptiveStatement(..)
    , BreakStatement(..)
    , ReturnStatement(..)
    ) where

data Value =
    Value [Statement] WhiteSpace
    deriving (Eq, Show)

data Statement =
    -- expression statement
    DisruptiveStatement WhiteSpace DisruptiveStatement
    -- try statement
    -- if statement
    -- (labeled) switch statement
    -- (labeled) while statement
    -- (labeled) for statement
    -- (labeled) do statement
    deriving (Eq, Show)

data DisruptiveStatement =
      BreakStatement       BreakStatement
    | ReturnStatement      ReturnStatement
    | ThrowStatement       WhiteSpace Expression
    deriving (Eq, Show)

data BreakStatement =
      EmptyBreakStatement   WhiteSpace
    | LabeledBreadStatement WhiteSpace String WhiteSpace
    deriving (Eq, Show)

data ReturnStatement =
      ExpressionReturnStatement WhiteSpace Expression
    | EmptyReturnStatement      WhiteSpace
    deriving (Eq, Show)

type Expression = String

type WhiteSpace = String
