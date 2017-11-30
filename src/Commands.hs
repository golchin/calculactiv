module Commands (
  RunHandler,
  Command(..),
  findCommand,
  quit,
  help
) where

import Common
import Utils

type RunHandler = [String] -> [Command] -> Result

data Command = Command {
  name :: String,
  description :: String,
  run :: RunHandler
}

findCommand :: String -> [Command] -> Maybe Command
findCommand nm cmds = head' [ x | x <- cmds, name x == nm ]

quit = Command {
  name = "quit",
  description = "Leave the program.",
  run = \_ _ -> Result {
    output = "Bye Bye!",
    continue = False
  }
}

help = Command {
  name = "help",
  description = "Learn more about calculactiv.",
  run = \_ cmds -> Result {
    output = unlines [name c ++ " - " ++ description c | c <- cmds],
    continue = True
  }
}
