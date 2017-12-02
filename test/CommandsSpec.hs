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
      out `shouldBe` "a\tCommand A\nb\tCommand B"
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
      out `shouldBe` "x\t10.0"
      cont `shouldBe` True

    it "should return error with few parameters" $ do
      -- act
      let res = run set ["set", "x"] [] []
      let store' = store res
      let out = output res
      let cont = continue res
      -- assert
      store' `shouldBe` []
      out `shouldBe` "Invalid usage, e.g., (set x 10)"
      cont `shouldBe` True

    it "should return error with invalid value" $ do
      -- act
      let res = run set ["set", "x", "foo"] [] []
      let store' = store res
      let out = output res
      let cont = continue res
      -- assert
      store' `shouldBe` []
      out `shouldBe` "Invalid value, e.g., (set x 10)"
      cont `shouldBe` True

  describe "vars" $ do

    it "should list all variables" $ do
      -- act
      let store = [("y", 5)]
      let res = run vars ["vars"] store []
      let out = output res
      let cont = continue res
      -- assert
      out `shouldBe` "y\t5.0"
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
