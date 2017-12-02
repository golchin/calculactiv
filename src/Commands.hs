module Commands (
  RunHandler,
  Command(..),
  findCommand,
  quit,
  help,
  set,
  vars
) where

import Text.Read
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
  run = runSet
}

runSet :: RunHandler
runSet [_,k,v] s _ = runSet' k (readMaybe v :: Maybe Float) s
runSet _ s _ = Result {
      store = s,
      output = "Invalid usage, e.g., (set x 10)",
      continue = True
    }

runSet' :: String -> Maybe Float -> Store -> Result
runSet' k Nothing s = Result {
      store = s,
      output = "Invalid value, e.g., (set x 10)",
      continue = True
    }
runSet' k v s = Result {
      store = addToStore s k val,
      output = k ++ "\t" ++ show val,
      continue = True
    }
  where val = fromJust v

vars = Command {
  name = "vars",
  description = "Lists all variables.",
  run = \_ s _ -> Result {
    store = s,
    output = (trim . unlines . fmap (\(k, v) -> k ++ "\t" ++ show v)) s,
    continue = True
  }
}
