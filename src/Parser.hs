module Parser (
  parseExpression
) where

import Text.Parsec (ParseError, parse, try)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (oneOf, char, digit, letter, satisfy, spaces)
import Text.Parsec.Combinator (many1, chainl1, eof, between)
import Control.Applicative ((<|>))
import Control.Monad (void, ap)
import Data.Char (isLetter, isDigit)
import Expressions

parseExpression :: String -> Either ParseError Expression
parseExpression exp = regularParse parseAll exp

parseConstant :: Parser Expression
parseConstant = do
  n <- many1 digit
  return (Constant $ read n)

parseVar :: Parser Expression
parseVar = do
  v <- many1 letter
  return (Var v)

parseParens :: Parser Expression -> Parser Expression
parseParens p = do
  e <- between (char '(') (char ')') p
  return (Parens e)

parseNegative :: Parser Expression
parseNegative = do
  char '-'
  e <- parseValue
  return (Negative e)

parseAddition :: Parser Expression
parseAddition = do
  l <- parseValue
  spaces
  char '+'
  spaces
  r <- parseAll
  return (Addition l r)

parseSubtraction :: Parser Expression
parseSubtraction = do
  l <- parseValue
  spaces
  char '-'
  spaces
  r <- parseAll
  return (Subtraction l r)

parseMultiplication :: Parser Expression
parseMultiplication = do
  l <- parseValue
  spaces
  char '*'
  spaces
  r <- parseAll
  return (Multiplication l r)

parseDivision :: Parser Expression
parseDivision = do
  l <- parseValue
  spaces
  char '/'
  spaces
  r <- parseAll
  return (Division l r)

parseValue :: Parser Expression
parseValue = (parseParens parseAll) <|> parseNegative <|> parseVar <|> parseConstant

parseOperator :: Parser Expression
parseOperator = try parseOperator1 <|> parseOperator2

parseOperator1 :: Parser Expression
parseOperator1 = try parseMultiplication <|> parseDivision

parseOperator2 :: Parser Expression
parseOperator2 = try parseAddition <|> parseSubtraction

parseAll :: Parser Expression
parseAll = try parseOperator <|> parseValue

regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p ""
