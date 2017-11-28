module ExpressionParser (
  Operator(..),
  Expression(..),
  parseExp
) where

import Text.Parsec (ParseError, parse, try)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (oneOf, char, digit, letter, satisfy, spaces)
import Text.Parsec.Combinator (many1, chainl1, eof, between)
import Control.Applicative ((<$>), (<*>), (<*), (*>), (<|>), many, (<$))
import Control.Monad (void, ap)
import Data.Char (isLetter, isDigit)

data Operator = Add | Subtract | Multiply | Divide deriving (Eq, Show)

data Expression = Constant Float
                | Var String
                | Parens Expression
                | Binary Operator Expression Expression
                  deriving (Eq, Show)

parseConstant :: Parser Expression
parseConstant = do
  spaces
  n <- many1 digit
  spaces
  return (Constant $ read n)

parseVar :: Parser Expression
parseVar = do
  spaces
  v <- many1 letter
  spaces
  return (Var v)

parseParens :: Parser Expression -> Parser Expression
parseParens p = do
  e <- between (char '(') (char ')') p
  return (Parens e)

parseBinary :: Parser Expression
parseBinary = do
  e1 <- term'
  op <- oneOf "+-/*"
  e2 <- term'
  return (Binary (getOperator op) e1 e2)

term' :: Parser Expression
term' = try parseVar <|> parseConstant <|> parseParens term

term :: Parser Expression
term = try parseBinary <|> term'

parseExp :: String -> Either ParseError Expression
parseExp exp = regularParse term exp

regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p ""

getOperator :: Char -> Operator
getOperator c
  | c == '+' = Add
  | c == '-' = Subtract
  | c == '*' = Multiply
  | c == '/' = Divide
