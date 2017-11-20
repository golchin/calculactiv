module EnvInteractifSpec where

import Test.Hspec
import EnvInteractif
import qualified Commands

spec :: Spec
spec = do

  describe "exec" $ do

    let context = EnvContext {
      supportedCommands = [ dummy ]
    }

    it "should execute the dummy command" $ do
      -- act
      let result = exec "dummy" context
      let msg = message result
      let cont = continue result
      -- assert
      msg `shouldBe` "What a dummy message :p"
      cont `shouldBe` True

    -- it "should evalute an expression" $ do
    --     -- act
    --     let res = exec "2 + 3"
    --     let msg = message res
    --     let exit = continue res
    --     -- assert
    --     msg `shouldBe` "5"
    --     exit `shouldBe` False

dummy = Commands.Command {
  Commands.name = "dummy",
  Commands.description = "A dummy command for test",
  Commands.invoke = doDummy
}

doDummy :: Commands.CommandContext -> Commands.CommandResult
doDummy ctx = Commands.CommandResult {
  Commands.message = "What a dummy message :p",
  Commands.continue = True
}
