module ExpressionEval (
  evalExp,
  evalStr,
  evalStr'
) where

import Common
import ExpressionParser
import Data.Maybe

evalExp :: Expression -> Store -> Maybe Float

evalExp (Parens x) store = evalExp x store

evalExp (Constant x) store = Just x

evalExp (Var x) store = fromStore store x

evalExp (Binary o x y) store
  | o == Add = Just (a + b)
  | o == Subtract = Just (a - b)
  | o == Multiply = Just (a * b)
  | o == Divide = Just (a / b)
  where
    a = fromJust (evalExp x store)
    b = fromJust (evalExp y store)

evalStr' :: String -> Store -> Result
evalStr' exp s = Result {
  output = show (fromJust (evalStr exp s)),
  store = s,
  continue = True
}

evalStr :: String -> Store -> Maybe Float
evalStr exp store = evalExp (parseExp' exp) store

parseExp' :: String -> Expression
parseExp' exp =
  case parseExp exp of
    Right e -> e
    Left _ -> Constant 0
