module CommandsSpec where

import Test.Hspec
import Commands

spec :: Spec
spec = do

  describe "quit" $ do
    it "should stop the program" $ do
      -- act
      let context = CommandContext {}
      let result = invoke quit context
      let cont = continue result
      -- assert
      cont `shouldBe` False
