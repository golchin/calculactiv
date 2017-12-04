module Main (
  main
) where

import System.IO
import Control.Monad
import EnvInteractif
import Expressions
import Commands

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
  let res = exec exp s
  putStrLn (output res)
  when (continue res) $ do
    readLine (store res)
