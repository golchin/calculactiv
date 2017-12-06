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
      -- assert
      (output res) `shouldBe` "Au revoir!"
      (continue res) `shouldBe` False

    it "should pass parameters to command" $ do
      -- act
      let result = exec "set x 5" []
      -- assert
      (store result) `shouldBe` [("x", 5)]

    it "should evaluate an expression" $ do
      -- act
      let result = exec "x + y" [("x", 2), ("y", 3)]
      -- assert
      (output result) `shouldBe` "5.0"
      (continue result) `shouldBe` True

    it "should show invalid expression" $ do
      -- act
      let result = exec "+ 3" []
      -- assert
      (output result) `shouldBe` "Expression invalide, par exemple, (2 + 2)."
      (continue result) `shouldBe` True

    it "should show undefined variable" $ do
      -- act
      let result = exec "x" []
      -- assert
      (output result) `shouldBe` "Variable indéfinie, définissez-la par la commande set, par exemple, (set x 10)."
      (continue result) `shouldBe` True
