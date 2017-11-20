module EnvInteractif (
  EnvContext(..),
  EnvResult(..),
  EvaluateExpressionHandler,
  exec
) where

import Data.Maybe
import qualified Commands
import Expressions
import Utils

type EvaluateExpressionHandler = String -> ExpressionResult

data EnvContext = EnvContext {
  supportedCommands :: [Commands.Command],
  evaluateExpression :: EvaluateExpressionHandler
}

data EnvResult = EnvResult {
  message :: String,
  continue :: Bool
} deriving (Eq, Show)

exec :: String -> EnvContext -> EnvResult
exec exp context
  | not $ isNothing cmd = transformCommandResult $ Commands.invoke (fromJust cmd) cmdContext
  | otherwise = transformExpressionResult $ evaluateExpression context exp
  where cmd = findCommand exp context -- find command by expression
        cmdContext = Commands.CommandContext {}

findCommand :: String -> EnvContext -> Maybe Commands.Command
findCommand nm ctx = head' [ x | x <- supportedCommands ctx, Commands.name x == nm ]

transformCommandResult :: Commands.CommandResult -> EnvResult
transformCommandResult res = EnvResult {
  message = Commands.message res,
  continue = Commands.continue res
}

transformExpressionResult :: ExpressionResult -> EnvResult
transformExpressionResult res = EnvResult {
  message = result res,
  continue = True
}
