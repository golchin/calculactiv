module Expressions where

import Data.Maybe

{--
  Store is a key/value pair to store variables
  --}
type Store = [(String, Float)]

{--
  Expression represents an expression tree for mathematical
  formulas
  --}
data Expression = Constant Float
                | Var String
                | Parens Expression
                | Addition Expression Expression
                | Subtraction Expression Expression
                | Multiplication Expression Expression
                | Division Expression Expression
                | Exponentiation Expression Expression
                | Sine Expression
                | Negative Expression
                  deriving (Eq, Show)

{--
  evaluates an expression tree
  --}
evalExpression :: Expression -> Store -> Maybe Float

evalExpression (Parens x) store = evalExpression x store

evalExpression (Constant x) store = Just x

evalExpression (Var x) [] = Nothing
evalExpression (Var x) ((k, v):store)
  | k == x = Just v
  | otherwise = evalExpression (Var x) store

evalExpression (Negative e) store =
  case evalExpression e store of
    Nothing -> Just 0
    (Just 0) -> Just 0
    (Just a) -> Just (-a)

evalExpression (Addition x y) store = do
    a <- evalExpression x store
    b <- evalExpression y store
    return (a + b)

evalExpression (Subtraction x y) store = do
    a <- evalExpression x store
    b <- evalExpression y store
    return (a - b)

evalExpression (Multiplication x y) store = do
    a <- evalExpression x store
    b <- evalExpression y store
    return (a * b)

evalExpression (Division x y) store = do
    a <- evalExpression x store
    b <- evalExpression y store
    return (a / b)

evalExpression (Exponentiation x y) store = do
    a <- evalExpression x store
    b <- evalExpression y store
    return (a ** b)

evalExpression (Sine x) store = do
    a <- evalExpression x store
    return (sin a)
