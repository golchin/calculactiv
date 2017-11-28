module Commands (
  RunHandler,
  Command(..),
  quit
) where

type RunHandler = [String] -> String

data Command = Command {
  name :: String,
  description :: String,
  exit :: Bool,
  run :: RunHandler
}

quit = Command {
  name = "quit",
  description = "Leave the program.",
  exit = True,
  run = \args -> "Bye Bye!"
}
