module Commands (
  RunHandler,
  Result(..),
  Command(..),
  findCommand,
  quit,
  help,
  set,
  vars
) where

import Text.Read
import Data.Maybe
import Expressions
import Utils

data Result = Result {
  store :: Store,
  output :: String,
  continue :: Bool
}

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

runSet [_,k,v] s _ =
  case val of
    Just x -> Result {
        store = (k, x):s,
        output = k ++ " = " ++ v,
        continue = True
      }
    Nothing -> Result {
        store = s,
        output = "Invalid value, e.g., (set x 10)",
        continue = True
      }
  where val = readMaybe v :: Maybe Float

runSet _ s _ = Result {
      store = s,
      output = "Invalid usage, e.g., (set x 10)",
      continue = True
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
