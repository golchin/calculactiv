module Commands (
  RunHandler,
  Command(..),
  quit,
  help
) where

type RunHandler = [String] -> [Command] -> String

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
  run = \_ _ -> "Bye Bye!"
}

help = Command {
  name = "help",
  description = "Learn more about calculactiv.",
  exit = False,
  run = \_ cmds -> unlines [name c ++ " - " ++ description c | c <- cmds]
}
