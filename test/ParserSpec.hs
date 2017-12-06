module ParserSpec where

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

    it "should parse an addition" $ do
      -- act
      let exp = parseExpression "2 + x + 6"
      -- assert
      exp `shouldBe` Right (Addition (Constant 2) (Addition (Var "x") (Constant 6)))

    it "should parse a subtraction" $ do
      -- act
      let exp = parseExpression "y - 3 - 2"
      -- assert
      exp `shouldBe` Right (Subtraction (Var "y") (Subtraction (Constant 3) (Constant 2)))

    it "should parse a multiplication" $ do
      -- act
      let exp = parseExpression "3 * x"
      -- assert
      exp `shouldBe` Right (Multiplication (Constant 3) (Var "x"))

    it "should parse a division" $ do
      -- act
      let exp = parseExpression "6 / 2"
      -- assert
      exp `shouldBe` Right (Division (Constant 6) (Constant 2))

    it "should parse an exponentiation" $ do
      -- act
      let exp = parseExpression "2 ^ 3"
      -- assert
      exp `shouldBe` Right (Exponentiation (Constant 2) (Constant 3))

    it "should parse complex formula" $ do
      -- act
      let exp = parseExpression "2 * 5 + 5"
      -- assert
      exp `shouldBe` Right (Multiplication (Constant 2) (Addition (Constant 5) (Constant 5)))

    it "should parse unary negetive operator" $ do
      -- act
      let exp = parseExpression "-5"
      -- assert
      exp `shouldBe` Right (Negative (Constant 5))

    it "should parse unary sin operator" $ do
      -- act
      let exp = parseExpression "sin 360"
      -- assert
      exp `shouldBe` Right (Sine (Constant 360))
