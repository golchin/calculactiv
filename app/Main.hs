module Main (main) where

import System.IO
import Control.Monad
import Data.Maybe
import Commands
import EnvInteractif
import ExpressionEval

commands = [quit, help]
evaluateExp = \exp -> show (fromJust (evalStr exp))
prompt = "> "

main :: IO()
main = do
  putStr prompt
  {-
    we've to flush the output buffer, becasue haskell flushes it
    at the end of the line
  -}
  hFlush stdout
  exp <- getLine
  let res = exec exp commands evaluateExp
  putStrLn (output res)
  when (continue res) $ do
    main
