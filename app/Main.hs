module Main (main) where

import System.IO
import Control.Monad
import EnvInteractif
import Commands

commands = [quit]
evalExp = \exp -> "An expression!"
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
  let res = exec exp commands evalExp
  putStrLn (output res)
  when (continue res) $ do
    main
