module EnvInteractif (
  EnvContext(..),
  EnvResult(..),
  exec
) where

import Data.Maybe
import qualified Commands
import Utils

data EnvContext = EnvContext {
  supportedCommands :: [Commands.Command]
}

data EnvResult = EnvResult {
  message :: String,
  continue :: Bool
} deriving (Eq, Show)

exec :: String -> EnvContext -> EnvResult
exec exp context
  | not $ isNothing cmd = transformResult $ Commands.invoke (fromJust cmd) cmdContext
  where cmd = findCommand exp context -- find command by expression
        cmdContext = Commands.CommandContext {}

findCommand :: String -> EnvContext -> Maybe Commands.Command
findCommand nm ctx = head' [ x | x <- supportedCommands ctx, Commands.name x == nm ]

transformResult :: Commands.CommandResult -> EnvResult
transformResult res = EnvResult {
  message = Commands.message res,
  continue = Commands.continue res
}
