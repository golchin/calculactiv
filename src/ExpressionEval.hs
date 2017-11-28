module ExpressionEval where

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
