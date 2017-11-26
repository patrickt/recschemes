module Part4Spec (spec) where

import           System.Random
import           Test.Hspec

import           Part4

spec :: Spec
spec = do
  describe "change()" $ do
    it "should return 1 given 1" $
      change 1 `shouldBe` 1

    it "should return 2 given 7" $
      change 7 `shouldBe` 2

    it "should return 4 given 17" $
      change 17 `shouldBe` 4

    it "should return 8 given 27" $
      change 27 `shouldBe` 8
