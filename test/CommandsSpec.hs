module CommandsSpec where

import Test.Hspec
import Commands

spec :: Spec
spec = do

  describe "quit" $ do

    it "should stop the program" $ do
      -- act
      let msg = run quit ["quit"] []
      let e = exit quit
      -- assert
      msg `shouldBe` "Bye Bye!"
      e `shouldBe` True

  describe "help" $ do

    it "should show descriptions" $ do
      -- act
      let commands = [a, b]
      let msg = run help ["help"] commands
      -- assert
      msg `shouldBe` "a - Command A\nb - Command B\n"

{--
  we've to define a some dummy commands for testing purpose
  --}
a = Command {
  name = "a",
  description = "Command A",
  exit = False,
  run = \_ _ -> ""
}

b = Command {
  name = "b",
  description = "Command B",
  exit = False,
  run = \_ _ -> ""
}
