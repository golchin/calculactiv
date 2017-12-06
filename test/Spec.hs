-- {-# OPTIONS_GHC -F -pgmF hspec-discover -optF --module-name=Main #-}

import Test.Hspec

import qualified EnvInteractifSpec
import qualified CommandsSpec
import qualified ExpressionsSpec
import qualified ParserSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "EnvInteractif"     EnvInteractifSpec.spec
  describe "Commands"          CommandsSpec.spec
  describe "Expressions"       ExpressionsSpec.spec
  describe "Parser"            ParserSpec.spec
