module CommandsSpec where

import Test.Hspec
import Common
import Commands

spec :: Spec
spec = do

  describe "quit" $ do

    it "should stop the program" $ do
      -- act
      let res = run quit ["quit"] [] []
      let out = output res
      let cont = continue res
      -- assert
      out `shouldBe` "Bye Bye!"
      cont `shouldBe` False

  describe "help" $ do

    it "should show descriptions" $ do
      -- act
      let commands = [a, b]
      let res = run help ["help"] [] commands
      let out = output res
      let cont = continue res
      -- assert
      out `shouldBe` "a - Command A\nb - Command B\n"
      cont `shouldBe` True

  describe "set" $ do

    it "should add a new variable in store" $ do
      -- act
      let res = run set ["set", "x", "10"] [] []
      let store' = store res
      let out = output res
      let cont = continue res
      -- assert
      store' `shouldBe` [("x", 10)]
      out `shouldBe` "x = 10"
      cont `shouldBe` True

{--
  we've to define a some dummy commands for testing purpose
  --}
a = Command {
  name = "a",
  description = "Command A",
  run = \_ s _ -> Result {
    store = s,
    output = "",
    continue = True
  }
}

b = Command {
  name = "b",
  description = "Command B",
  run = \_ s _ -> Result {
    store = s,
    output = "",
    continue = True
  }
}
