import Control.Monad
import JSRefactor.JS.Parser
import JSRefactor.JS.Printer
import JSRefactor.JS.Types
import Test.QuickCheck
import Test.QuickCheck.Arbitrary

-- Generators

whitespace    = listOf  oneWhitespace
reqWhitespace = listOf1 oneWhitespace
oneWhitespace = elements " \n"

instance Arbitrary StringLiteral where
    arbitrary = oneof [ liftM DoubleQuotedString innerString
                      , liftM SingleQuotedString innerString
                      ]
        where innerString = listOf (elements (['"', '\'', '\\', '/', '\b', '\f', '\n', '\r', '\t'] ++ ['a'..'z']  ++ [' '..'ω']))
    shrink (DoubleQuotedString s) = map DoubleQuotedString $ shrinkList shrink s
    shrink (SingleQuotedString s) = map SingleQuotedString $ shrinkList shrink s

instance Arbitrary Literal where
    arbitrary = oneof [ liftM  NumberLiteral number
                      , liftM  StringLiteral arbitrary
                      ]
        where number          = integer <++> fraction <++> exponent
              integer         = oneof [ return "0"
                                      , liftM2 (:) nonZeroDigit (listOf digit)
                                      ]
              fraction        = oneof [ return ""
                                      , liftM2 (++) (return ".") (listOf digit)
                                      ]
              exponent        = (elements ["e", "E"]) <++> (elements ["", "+", "-"]) <++> (listOf1 digit)
              nonZeroDigit    = elements "123456789"
              digit           = elements "1234567890"
    shrink (NumberLiteral _)   = []
    shrink (StringLiteral lit) = map StringLiteral $ shrink lit

instance Arbitrary Expression where
    arbitrary = oneof [ liftM  LiteralExpression arbitrary
                      ]
    shrink (LiteralExpression litExpr) = map LiteralExpression $ shrink litExpr

instance Arbitrary ReturnStatement where
    arbitrary = oneof [ liftM3 ExpressionReturnStatement reqWhitespace arbitrary whitespace
                      , liftM  EmptyReturnStatement      whitespace
                      ]
    shrink (ExpressionReturnStatement s1 expr s2) = [ExpressionReturnStatement s1 expr' s2 | expr' <- shrink expr]
    shrink _                                      = []

instance Arbitrary BreakStatement where
    arbitrary = oneof [ liftM3 LabeledBreadStatement reqWhitespace label whitespace
                      , liftM  EmptyBreakStatement   whitespace
                      ]
        where label = return "abc"

instance Arbitrary DisruptiveStatement where
    arbitrary = oneof [ liftM  BreakStatement  arbitrary
                      , liftM  ReturnStatement arbitrary
                      , liftM3 ThrowStatement  reqWhitespace arbitrary whitespace
                      ]
    shrink (BreakStatement break)      = map BreakStatement $ shrink break
    shrink (ReturnStatement ret)       = map ReturnStatement $ shrink ret
    shrink (ThrowStatement s1 expr s2) = [ThrowStatement s1 expr' s2 | expr' <- shrink expr]

instance Arbitrary Statement where
    arbitrary = oneof [ liftM  DisruptiveStatement arbitrary
                      ]
    shrink (DisruptiveStatement disruptive) = map DisruptiveStatement $ shrink disruptive

instance Arbitrary Statements where
    arbitrary = oneof [ liftM3 Statement    whitespace arbitrary arbitrary
                      , liftM  EndStatement whitespace
                      ]
    shrink stmts = [fromList l (endWhiteSpace stmts) |
                    l <- shrinkList (\(s, stmt) -> [(s, stmt') |
                                                    stmt' <- shrink stmt]) (toList stmts)]
        where toList :: Statements -> [(WhiteSpace, Statement)]
              toList (Statement s stmt stmts) = (s, stmt) : toList stmts
              toList _                        = []

              endWhiteSpace :: Statements -> WhiteSpace
              endWhiteSpace (EndStatement s)      = s
              endWhiteSpace (Statement _ _ stmts) = endWhiteSpace stmts

              fromList :: [(WhiteSpace, Statement)] -> WhiteSpace -> Statements
              fromList ((s, stmt) : stmts) end = Statement s stmt (fromList stmts end)
              fromList []                  end = EndStatement end

instance Arbitrary Value where
    arbitrary = liftM Value arbitrary
    shrink (Value stmts) = map Value $ shrink stmts

(<++>) :: Gen String -> Gen String -> Gen String
first <++> second = liftM2 (++) first second

-- Properties

prop_parsed_and_printed_is_same_as_original value =
    let originalString = printValue value in
        case parseJSFile originalString of
            Left  _      -> False
            Right value' -> (printValue value') == originalString

prop_printed_and_parsed_is_same_as_original value =
    case parseJSFile (printValue value) of
        Left  _      -> False
        Right value' -> value' == value

-- Runner

main = do
    quickCheckWith (stdArgs { maxSize = 5 }) prop_parsed_and_printed_is_same_as_original
    quickCheckWith (stdArgs { maxSize = 5 }) prop_printed_and_parsed_is_same_as_original
