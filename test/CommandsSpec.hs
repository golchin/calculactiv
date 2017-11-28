module CommandsSpec where

import Test.Hspec
import Commands

spec :: Spec
spec = do

  describe "quit" $ do

    it "should stop the program" $ do
      -- act
      let msg = run quit ["quit"]
      let e = exit quit
      -- assert
      msg `shouldBe` "Bye Bye!"
      e `shouldBe` True
