module Commands (
  RunHandler,
  Command(..),
  findCommand,
  quit,
  help,
  set,
  vars,
  get
) where

import Data.Maybe
import Common
import Utils

type RunHandler = [String] -> Store -> [Command] -> Result

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
  run = \_ s _ -> Result {
    store = s,
    output = "Bye Bye!",
    continue = False
  }
}

help = Command {
  name = "help",
  description = "Learn more about calculactiv.",
  run = \_ s cmds -> Result {
    store = s,
    output = (trim . unlines . fmap (\c -> name c ++ "\t" ++ description c)) cmds,
    continue = True
  }
}

set = Command {
  name = "set",
  description = "Add a new variable.",
  run = \[_,k,v] s _ -> Result {
    store = (k, read v :: Float):s,
    output = k ++ "\t" ++ v,
    continue = True
  }
}

vars = Command {
  name = "vars",
  description = "Lists all variables.",
  run = \_ s _ -> Result {
    store = s,
    output = (trim . unlines . fmap (\(k, v) -> k ++ "\t" ++ show v)) s,
    continue = True
  }
}

get = Command {
  name = "get",
  description = "Gets the variable value.",
  run = \[_, k] s _ -> Result {
    store = s,
    output = show $ fromMaybe 0 (fromStore s k),
    continue = True
  }
}
