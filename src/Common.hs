module Common where

type Store = [(String, Float)]

data Result = Result {
  store :: Store,
  output :: String,
  continue :: Bool
}

type ExpressionEval = String -> Store -> Result
