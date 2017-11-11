module Main (main) where

import System.IO
import Control.Monad

comment = unlines $ [ "Type a mathematical expression to evaluate, or",
                       "use 'help' to learn more" ]
prompt = "> "

main :: IO()
main = do
  putStrLn comment
  readLine

readLine :: IO()
readLine = do
  -- we've to flush the output buffer, becasue haskell flushes it
  -- at the end of the line
  putStr prompt
  hFlush stdout
  arg <- getLine
  when (arg /= "quit") $ do
    putStrLn "Command..."
    readLine
