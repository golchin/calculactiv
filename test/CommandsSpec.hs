module CommandsSpec where

import Test.Hspec
import Commands
import Expressions

spec :: Spec
spec = do

  describe "quit" $ do

    it "should stop the program" $ do
      -- act
      let res = run quit ["quit"] [] []
      -- assert
      (output res) `shouldBe` "Au revoir!"
      (continue res) `shouldBe` False

  describe "help" $ do

    it "should show descriptions" $ do
      -- act
      let res = run help ["help"] [] [a, b]
      -- assert
      (output res) `shouldBe` "a\tCommand A\nb\tCommand B"
      (continue res) `shouldBe` True

  describe "set" $ do

    it "should add a new variable in store" $ do
      -- act
      let res = run set ["set", "x", "10"] [] []
      -- assert
      (store res) `shouldBe` [("x", 10)]
      (output res) `shouldBe` "x = 10"
      (continue res) `shouldBe` True

    it "should return error with few parameters" $ do
      -- act
      let res = run set ["set", "x"] [] []
      -- assert
      (store res) `shouldBe` []
      (output res) `shouldBe` "Utilisation non valide, par exemple, (set x 10)"
      (continue res) `shouldBe` True

    it "should return error with invalid value" $ do
      -- act
      let res = run set ["set", "x", "foo"] [] []
      -- assert
      (store res) `shouldBe` []
      (output res) `shouldBe` "Valeur non valide, par exemple, (set x 10)"
      (continue res) `shouldBe` True

  describe "unset" $ do

    it "should remove a variable from store" $ do
      -- act
      let res = run unset ["unset", "y"] [("x", 1), ("y", 2), ("z", 3)] []
      -- assert
      (store res) `shouldBe` [("x", 1), ("z", 3)]
      (output res) `shouldBe` "y supprimé."
      (continue res) `shouldBe` True

  describe "vars" $ do

    it "should list all variables" $ do
      -- act
      let res = run vars ["vars"] [("y", 5)] []
      -- assert
      (output res) `shouldBe` "y\t5.0"
      (continue res) `shouldBe` True

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
