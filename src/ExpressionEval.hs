module ExpressionEval (
  evalExp,
  evalStr
) where

import ExpressionParser
import Data.Maybe

evalExp :: Expression -> Maybe Float

evalExp (Parens x) = evalExp x

evalExp (Constant x) = Just x

evalExp (Binary o x y)
  | o == Add = Just (a + b)
  | o == Subtract = Just (a - b)
  | o == Multiply = Just (a * b)
  | o == Divide = Just (a / b)
  where
    a = fromJust (evalExp x)
    b = fromJust (evalExp y)

evalStr :: String -> Maybe Float
evalStr exp = evalExp (parseExp' exp)

parseExp' :: String -> Expression
parseExp' exp =
  case parseExp exp of
    Right e -> e
    Left _ -> Constant 0
