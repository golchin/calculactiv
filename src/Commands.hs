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
  abbreviation :: String,
  description :: String,
  run :: RunHandler
}

findCommand :: String -> [Command] -> Maybe Command
findCommand nm cmds = head' [ x | x <- cmds, name x == nm || abbreviation x == nm ]

quit = Command {
  name = "quit",
  abbreviation = "q",
  description = "Quittez le programme.",
  run = \_ s _ -> Result {
    store = s,
    output = "Au revoir!",
    continue = False
  }
}

help = Command {
  name = "help",
  abbreviation = "h",
  description = "En savoir plus sur calculactiv.",
  run = \_ s cmds -> Result {
    store = s,
    output = (trim . unlines . fmap (formatHelp)) cmds,
    continue = True
  }
}

unsetAll = Command {
  name = "unsetAll",
  abbreviation = "usall",
  description = "Supprimer toutes les variables.",
  run = \_ s _ -> Result {
    store = [],
    output = "Tout supprimés.",
    continue = True
  }
}

unset = Command {
  name = "unset",
  abbreviation = "us",
  description = "Supprimer une variable",
  run = runUnset
}

runUnset :: RunHandler
runUnset [_, k] s _ = Result {
    store = removeFromStore k s,
    output = k ++ " supprimé.",
    continue = True
  }
runUnset _ s _ = Result {
    store = s,
    output = "Utilisation non valide, par exemple, (unset x)",
    continue = True
  }

set = Command {
  name = "set",
  abbreviation = "s",
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
  abbreviation = "v",
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

formatHelp :: Command -> String
formatHelp c = name c ++ " - " ++ abbreviation c ++ "\n\t" ++ description c
