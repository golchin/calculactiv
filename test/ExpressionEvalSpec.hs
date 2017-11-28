module ExpressionEvalSpec where

import Test.Hspec
import ExpressionParser
import ExpressionEval

spec :: Spec
spec = do

  describe "evalExp" $ do

    it "should return number between parens" $ do
      -- act
      let exp = Parens (Constant 7)
      let res = evalExp exp
      -- assert
      res `shouldBe` Just 7

    it "should return 10" $ do
      -- act
      let exp = Constant 10
      let res = evalExp exp
      -- assert
      res `shouldBe` Just 10

    it "should return sum of 2 and 3" $ do
      -- act
      let exp = Binary Add (Constant 2) (Constant 3)
      let res = evalExp exp
      -- assert
      res `shouldBe` Just 5

    it "should return subtract of 10 and 6" $ do
      -- act
      let exp = Binary Subtract (Constant 10) (Constant 6)
      let res = evalExp exp
      -- assert
      res `shouldBe` Just 4

    it "should return multiply of 2 by 3" $ do
      -- act
      let exp = Binary Multiply (Constant 2) (Constant 3)
      let res = evalExp exp
      -- assert
      res `shouldBe` Just 6

    it "should return divide of 100 by 5" $ do
      -- act
      let exp = Binary Divide (Constant 100) (Constant 5)
      let res = evalExp exp
      -- assert
      res `shouldBe` Just 20

  describe "evalStr" $ do

    it "should return multiply of 10 by 5" $ do
      -- act
      let exp = "(10 * 5)"
      let res = evalStr exp
      -- assert
      res `shouldBe` Just 50
