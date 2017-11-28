module EnvInteractifSpec where

import Test.Hspec
import EnvInteractif
import Commands

spec :: Spec
spec = do

  describe "exec" $ do

    let commands = [dummy, sum']

    it "should execute the dummy command" $ do
      -- act
      let res = exec "dummy" commands
      let msg = output res
      let cont = continue res
      -- assert
      msg `shouldBe` "What a dummy message :p"
      cont `shouldBe` False

    it "should pass parameters to command" $ do
      -- act
      let result = exec "sum 2 5" commands
      let msg = output result
      let cont = continue result
      -- assert
      msg `shouldBe` "25"
      cont `shouldBe` True

    -- it "should evalute an expression" $ do
    --   -- act
    --   let result = exec "2 + 3" commands
    --   let msg = message result
    --   let cont = continue result
    --   -- assert
    --   msg `shouldBe` "5"
    --   cont `shouldBe` True


-- we've to define a some dummy commands for testing purpose

dummy = Command {
  name = "dummy",
  description = "A dummy command for test",
  exit = True,
  run = \args -> "What a dummy message :p"
}

sum' = Command {
  name = "sum",
  description = "Add two numbers together",
  exit = False,
  run = runSum
}

runSum :: [String] -> String
runSum [] = "0"
runSum [a,b] = a ++ b
runSum [_,a,b] = a ++ b
