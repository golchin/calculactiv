module EnvInteractif (
  exec
) where

import Data.Maybe
import Data.List.Split
import Parser
import Expressions
import Commands

supportedCommands = [quit, help, set, unset, unsetAll, vars]

exec :: String -> Store -> Result
exec exp s
  {--
    first, we've to check wether it's a command call or not, if so then we
    should run the command and create and return the result.
    --}
  | isCmd = run cmd args s supportedCommands
  {--
    otherwise, it should be an expression, so we've to evaluate it then
    create and return the result.
    --}
  | otherwise = evalStringExpression exp s
  {--
    we've to split the expression with spaces to create a list of command
    name and params. (e.g. add 2 5)
    this example illustrates a command called add and its two parameters
    2 and 5.
    --}
  where args = splitOn " " exp
        name = head args
        findRes = findCommand name supportedCommands
        isCmd = not (isNothing findRes)
        cmd = fromJust findRes

evalStringExpression :: String -> Store -> Result
evalStringExpression str s = Result {
      store = s,
      output = out,
      continue = True
    }
  where
    exp = parseExpression str
    out = case exp of
      Right e -> case (evalExpression e s) of
        Nothing -> "Variable indéfinie, définissez-la par la commande set, par exemple, (set x 10)."
        Just x -> show x
      Left _ -> "Expression invalide, par exemple, (2 + 2)."
