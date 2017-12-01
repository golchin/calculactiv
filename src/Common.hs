module Common where

import Data.Maybe
import Utils

type Store = [(String, Float)]

fromStore :: Store -> String -> Maybe Float
fromStore s n = head' [v | (k, v) <- s, k == n]

data Result = Result {
  store :: Store,
  output :: String,
  continue :: Bool
}

type ExpressionEval = String -> Store -> Result
