module Common where

import Data.Maybe
import Utils

type Store = [(String, Float)]

fromStore :: Store -> String -> Float
fromStore s n = fromMaybe 0 (head' [v | (k, v) <- s, k == n])

addToStore :: Store -> String -> Float -> Store
addToStore store name value = (name, value):store

data Result = Result {
  store :: Store,
  output :: String,
  continue :: Bool
}

type ExpressionEval = String -> Store -> Result
