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
      let out = output res
      let cont = continue res
      -- assert
      out `shouldBe` "Au revoir!"
      cont `shouldBe` False

    it "should pass parameters to command" $ do
      -- act
      let result = exec "set x 5" []
      let s = store result
      -- assert
      s `shouldBe` [("x", 5)]

    it "should evaluate an expression" $ do
      -- act
      let result = exec "x + y" [("x", 2), ("y", 3)]
      let out = output result
      let cont = continue result
      -- assert
      out `shouldBe` "5.0"
      cont `shouldBe` True

    it "should show invalid expression" $ do
      -- act
      let result = exec "+ 3" []
      let out = output result
      let cont = continue result
      -- assert
      out `shouldBe` "Expression invalide, par exemple, (2 + 2)."
      cont `shouldBe` True

    it "should show undefined variable" $ do
      -- act
      let result = exec "x" []
      let out = output result
      let cont = continue result
      -- assert
      out `shouldBe` "Variable indéfinie, définissez-la par la commande set, par exemple, (set x 10)."
      cont `shouldBe` True
