module Commands (
  RunHandler,
  Result(..),
  Command(..),
  findCommand,
  quit,
  help,
  set,
  unset,
  unsetAll,
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
  description = "Quittez le programme.",
  run = \_ s _ -> Result {
    store = s,
    output = "Au revoir!",
    continue = False
  }
}

help = Command {
  name = "help",
  description = "En savoir plus sur calculactiv.",
  run = \_ s cmds -> Result {
    store = s,
    output = (trim . unlines . fmap (\c -> name c ++ "\t" ++ description c)) cmds,
    continue = True
  }
}

unsetAll = Command {
  name = "unsetAll",
  description = "Supprimer toutes les variables.",
  run = \_ s _ -> Result {
    store = [],
    output = "Tout supprimés",
    continue = True
  }
}

unset = Command {
  name = "unset",
  description = "Supprimer une variable",
  run = \[_, k] s _ -> Result {
    store = removeFromStore k s,
    output = k ++ " supprimé.",
    continue = True
  }
}

set = Command {
  name = "set",
  description = "Ajouter une nouvelle variable.",
  run = runSet
}

runSet :: RunHandler
runSet [_,k,v] s _ =
  case val of
    Just x -> Result {
        store = (k, x):(removeFromStore k s),
        output = k ++ " = " ++ v,
        continue = True
      }
    Nothing -> Result {
        store = s,
        output = "Valeur non valide, par exemple, (set x 10)",
        continue = True
      }
  where val = readMaybe v :: Maybe Float

runSet _ s _ = Result {
      store = s,
      output = "Utilisation non valide, par exemple, (set x 10)",
      continue = True
    }

vars = Command {
  name = "vars",
  description = "Répertorie toutes les variables.",
  run = \_ s _ -> Result {
    store = s,
    output = (trim . unlines . fmap (\(k, v) -> k ++ "\t" ++ show v)) s,
    continue = True
  }
}

removeFromStore :: String -> Store -> Store
removeFromStore n [] = []
removeFromStore n (val@(x, y):xs)
    | n == x = removeFromStore n xs
    | otherwise = val:removeFromStore n xs
