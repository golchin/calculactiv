module Commands (
  CommandContext(..),
  CommandResult(..),
  Handler,
  Command(..),
  quit,
) where

data CommandContext = CommandContext { }

data CommandResult = CommandResult {
  message :: String,
  continue :: Bool
}

type Handler = CommandContext -> CommandResult

data Command = Command {
  name :: String,
  description :: String,
  invoke :: Handler
}

quit = Command {
  name = "quit",
  description = "Leave the program.",
  invoke = doQuit
}

doQuit :: CommandContext -> CommandResult
doQuit ctx = CommandResult {
  message = "",
  continue = False
}
