module Common where

type Store = [(String, Float)]

data Result = Result {
  output :: String,
  continue :: Bool
}

type ExpressionEval = String -> Result
