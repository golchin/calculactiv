module ExpressionParserSpec where

import Test.Hspec
import ExpressionParser

spec :: Spec
spec = do

  describe "parse" $ do

    it "should parse a constant" $ do
      -- act
      let exp = parseExp "10"
      -- assert
      exp `shouldBe` Right (Constant 10)

    it "should parse a variable" $ do
      -- act
      let exp = parseExp "x"
      -- assert
      exp `shouldBe` Right (Var "x")

    it "should parse a parens" $ do
      -- act
      let exp = parseExp "(5)"
      -- assert
      exp `shouldBe` Right (Parens (Constant 5))

    it "should parse an add" $ do
      -- act
      let exp = parseExp "2 + 5"
      -- assert
      exp `shouldBe` Right (Binary Add (Constant 2) (Constant 5))

    it "should parse a subtract" $ do
      -- act
      let exp = parseExp "10 - 3"
      -- assert
      exp `shouldBe` Right (Binary Subtract (Constant 10) (Constant 3))

    it "should parse a multiply" $ do
      -- act
      let exp = parseExp "3 * x"
      -- assert
      exp `shouldBe` Right (Binary Multiply (Constant 3) (Var "x"))

    it "should parse a divide" $ do
      -- act
      let exp = parseExp "y / 2"
      -- assert
      exp `shouldBe` Right (Binary Divide (Var "y") (Constant 2))
