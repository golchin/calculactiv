module EnvInteractifSpec where

import Test.Hspec
import EnvInteractif
import qualified Commands
import Expressions

spec :: Spec
spec = do

  describe "exec" $ do

    let context = EnvContext {
      supportedCommands = [ dummy ],
      evaluateExpression = stubEvaluateExpression
    }

    it "should execute the dummy command" $ do
      -- act
      let result = exec "dummy" context
      let msg = message result
      let cont = continue result
      -- assert
      msg `shouldBe` "What a dummy message :p"
      cont `shouldBe` True

    it "should evalute an expression" $ do
        -- act
        let result = exec "2 + 3" context
        let msg = message result
        let cont = continue result
        -- assert
        msg `shouldBe` "5"
        cont `shouldBe` True


-- we've to define a dummy command for testing purpose

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

-- and a stub implementation of evaluateExpression function

stubEvaluateExpression :: String -> ExpressionResult
stubEvaluateExpression exp = ExpressionResult {
  result = "5"
}
