module EnvInteractif (
  ExpressionEval,
  Result(..),
  exec
) where

import Data.Maybe
import Common
import Commands
import Utils
import Data.List.Split

exec :: String -> [Command] -> ExpressionEval -> Result
exec exp cmds eval
  {--
    first, we've to check wether it's a command call or not, if so then we
    should run the command and create and return the result.
    --}
  | isCmd = run cmd args cmds
  {--
    otherwise, it should be an expression, so we've to evaluate it then
    create and return the result.
    --}
  | otherwise = eval exp
  {--
    we've to split the expression with spaces to create a list of command
    name and params. (e.g. add 2 5)
    this example illustrates a command called add and its two parameters
    2 and 5.
    --}
  where args = splitOn " " exp
        name = head args
        findRes = findCommand name cmds
        cmd = fromJust findRes
        isCmd = not (isNothing findRes)

findCommand :: String -> [Command] -> Maybe Command
findCommand nm cmds = head' [ x | x <- cmds, name x == nm ]
