module JSRefactor.JS.Types
    (
      Value(..)
    , Statements(..)
    , Statement(..)
    , DisruptiveStatement(..)
    , BreakStatement(..)
    , ReturnStatement(..)
    , Expression(..)
    , Literal(..)
    , StringLiteral(..)
    ) where

data Value =
    Value Statements
    deriving (Eq, Show)

data Statements =
      Statement    WhiteSpace Statement Statements
    | EndStatement WhiteSpace
    deriving (Eq, Show)

data Statement =
    -- expression statement
    DisruptiveStatement DisruptiveStatement
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
    | ThrowStatement       WhiteSpace Expression WhiteSpace
    deriving (Eq, Show)

data BreakStatement =
      EmptyBreakStatement   WhiteSpace
    | LabeledBreadStatement WhiteSpace String WhiteSpace
    deriving (Eq, Show)

data ReturnStatement =
      ExpressionReturnStatement WhiteSpace Expression WhiteSpace
    | EmptyReturnStatement      WhiteSpace
    deriving (Eq, Show)

data Expression =
    LiteralExpression Literal
    -- name
    -- paren expression
    -- prefix expression
    -- infix expression
    -- ?: expression
    -- invocation
    -- refinement
    -- new
    -- delete
    deriving (Eq, Show)

data Literal =
      NumberLiteral String
    | StringLiteral StringLiteral
    -- object
    -- array
    -- function
    -- regexp
    deriving (Eq, Show)

data StringLiteral =
      DoubleQuotedString String
    | SingleQuotedString String
    deriving (Eq, Show)

type WhiteSpace = String
