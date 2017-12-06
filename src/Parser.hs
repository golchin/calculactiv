module Parser (
  parseExpression
) where

import Text.Parsec (ParseError, parse, try)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (oneOf, char, digit, letter, spaces)
import Text.Parsec.Combinator (many1, between)
import Control.Applicative ((<|>))
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

parseParens :: Parser Expression
parseParens = do
  e <- between (char '(') (char ')') parseAll
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

parseExponentiation :: Parser Expression
parseExponentiation = do
  l <- parseValue
  spaces
  char '^'
  spaces
  r <- parseAll
  return (Exponentiation l r)

parseValue :: Parser Expression
parseValue = try parseParens <|> parseNegative <|> parseVar <|> parseConstant

parseOperator :: Parser Expression
parseOperator = try parseExponentiation <|>
                try parseMultiplication <|>
                try parseDivision <|>
                try parseAddition <|>
                try parseSubtraction

parseAll :: Parser Expression
parseAll = try parseOperator <|> parseValue

regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p ""
