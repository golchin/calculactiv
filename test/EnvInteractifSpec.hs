module EnvInteractifSpec where

import Test.Hspec
import EnvInteractif
import Common
import Commands

spec :: Spec
spec = do

  describe "exec" $ do

    let commands = [dummy, add]
    let dummyExpEval = \exp -> Result { output = "", continue = True }

    it "should execute the dummy command" $ do
      -- act
      let res = exec "dummy" commands dummyExpEval
      let msg = output res
      let cont = continue res
      -- assert
      msg `shouldBe` "What a dummy message :p"
      cont `shouldBe` False

    it "should pass parameters to command" $ do
      -- act
      let result = exec "add 2 5" commands dummyExpEval
      let msg = output result
      let cont = continue result
      -- assert
      msg `shouldBe` "7"
      cont `shouldBe` True

    it "should evaluate an expression" $ do
      -- act
      let stubExpEval = \exp -> Result { output = "5", continue = True }
      let result = exec "2 + 3" commands stubExpEval
      let msg = output result
      let cont = continue result
      -- assert
      msg `shouldBe` "5"
      cont `shouldBe` True

{--
  we've to define a some dummy commands for testing purpose
  --}
dummy = Command {
  name = "dummy",
  description = "A dummy command for test",
  run = \args _ -> Result {
    output = "What a dummy message :p",
    continue = False
  }
}

add = Command {
  name = "add",
  description = "Add two numbers together",
  run = \[_,a,b] _ -> Result {
    output = show ((read a :: Int) + (read b :: Int)),
    continue = True
  }
}
