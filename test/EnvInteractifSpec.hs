module EnvInteractifSpec where

import Test.Hspec
import EnvInteractif
import Commands

spec :: Spec
spec = do

  describe "exec" $ do

    it "should execute the quit command" $ do
      -- act
      let res = exec "quit" []
      let msg = output res
      let cont = continue res
      -- assert
      msg `shouldBe` "Bye Bye!"
      cont `shouldBe` False

    it "should pass parameters to command" $ do
      -- act
      let result = exec "set x 5" []
      let s = store result
      -- assert
      s `shouldBe` [("x", 5)]

    it "should evaluate an expression" $ do
      -- act
      let result = exec "x+y" [("x", 2), ("y", 3)]
      let msg = output result
      let cont = continue result
      -- assert
      msg `shouldBe` "5.0"
      cont `shouldBe` True
