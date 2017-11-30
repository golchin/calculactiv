module Main (main) where

import System.IO
import Control.Monad
import Data.Maybe
import Common
import Commands
import EnvInteractif
import ExpressionEval

commands = [quit, help]
prompt = "> "

main :: IO()
main = readLine []

readLine :: Store -> IO()
readLine s = do
  putStr prompt
  {-
    we've to flush the output buffer, becasue haskell flushes it
    at the end of the line
  -}
  hFlush stdout
  exp <- getLine
  let res = exec exp s commands evalStr'
  putStrLn (output res)
  when (continue res) $ do
    readLine (store res)
