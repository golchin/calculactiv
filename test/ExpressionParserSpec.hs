module ExpressionParserSpec where

import Test.Hspec
import Expressions
import Parser

spec :: Spec
spec = do

  describe "parseExpression" $ do

    it "should parse a constant" $ do
      -- act
      let exp = parseExpression "10"
      -- assert
      exp `shouldBe` Right (Constant 10)

    it "should parse a variable" $ do
      -- act
      let exp = parseExpression "x"
      -- assert
      exp `shouldBe` Right (Var "x")

    it "should parse a parens" $ do
      -- act
      let exp = parseExpression "(5)"
      -- assert
      exp `shouldBe` Right (Parens (Constant 5))

    it "should parse an add" $ do
      -- act
      let exp = parseExpression "2 + 5"
      -- assert
      exp `shouldBe` Right (Addition (Constant 2) (Constant 5))

    it "should parse a subtract" $ do
      -- act
      let exp = parseExpression "10 - 3"
      -- assert
      exp `shouldBe` Right (Subtraction (Constant 10) (Constant 3))

    it "should parse a multiply" $ do
      -- act
      let exp = parseExpression "3 * x"
      -- assert
      exp `shouldBe` Right (Multiplication (Constant 3) (Var "x"))

    it "should parse a divide" $ do
      -- act
      let exp = parseExpression "6 / 2"
      -- assert
      exp `shouldBe` Right (Division (Constant 6) (Constant 2))

    -- it "should parse unary negetive operator" $ do
    --   -- act
    --   let exp = parseExpression "-5"
    --   -- assert
    --   exp `shouldBe` Right (Negative (Constant 5))
