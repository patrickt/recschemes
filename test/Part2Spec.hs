module Part2Spec (spec) where

import Part2
import Test.Hspec

spec :: Spec
spec = do
  describe "countNodes" $
    it "should yield 4 on add(10, 10)" $
      cata countNodes call `shouldBe` 4

  describe "prettyPrint" $
    it "should yield add(10,10)" $
      show (cata prettyPrint call) `shouldBe` "add(10,10)"
