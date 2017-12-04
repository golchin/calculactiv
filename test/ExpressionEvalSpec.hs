module ExpressionEvalSpec where

import Test.Hspec
import Expressions
import Parser

spec :: Spec
spec = do

  describe "evalExpression" $ do

    it "should return number between parens" $ do
      -- act
      let exp = Parens (Constant 7)
      let res = evalExpression exp []
      -- assert
      res `shouldBe` Just 7

    it "should return 10" $ do
      -- act
      let exp = Constant 10
      let res = evalExpression exp []
      -- assert
      res `shouldBe` Just 10

    it "should return sum of 2 and 3" $ do
      -- act
      let exp = Addition (Constant 2) (Constant 3)
      let res = evalExpression exp []
      -- assert
      res `shouldBe` Just 5

    it "should return subtract of 10 and 6" $ do
      -- act
      let exp = Subtraction (Constant 10) (Constant 6)
      let res = evalExpression exp []
      -- assert
      res `shouldBe` Just 4

    it "should return multiply of 2 by 3" $ do
      -- act
      let exp = Multiplication (Constant 2) (Constant 3)
      let res = evalExpression exp []
      -- assert
      res `shouldBe` Just 6

    it "should return divide of 100 by 5" $ do
      -- act
      let exp = Division (Constant 100) (Constant 5)
      let res = evalExpression exp []
      -- assert
      res `shouldBe` Just 20

    it "should evaluate variable" $ do
      -- act
      let store = [("x", 5), ("y", 3)]
      let exp = Addition (Var "x") (Var "y")
      let res = evalExpression exp store
      -- assert
      res `shouldBe` Just 8

    it "should return 0 when variable does not exist" $ do
      -- act
      let exp = Var "x"
      let res = evalExpression exp []
      -- assert
      res `shouldBe` Nothing

    it "should return evaluate negetive number" $ do
      -- act
      let exp = Negative (Constant 5)
      let res = evalExpression exp []
      -- assert
      res `shouldBe` Just (-5)
