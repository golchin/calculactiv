module EnvInteractif (
  Result(..),
  exec
) where

import Data.Maybe
import Commands
import Expressions
import Utils
import Data.List.Split

data Result = Result {
  output :: String,
  continue :: Bool
}

exec :: String -> [Command] -> Result
exec exp cmds
  | isCmd = Result { output = run cmd args, continue = not (exit cmd) }
  -- | otherwise = evaluateExpression context exp
  where args = splitOn " " exp
        name = head args
        res = findCommand name cmds
        cmd = fromJust res
        isCmd = not (isNothing res)

findCommand :: String -> [Command] -> Maybe Command
findCommand nm cmds = head' [ x | x <- cmds, name x == nm ]
