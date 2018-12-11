module Main where

import qualified Part1Spec
import qualified Part2Spec
import qualified Part3Spec
import           Test.Hspec
import Part1

main :: IO ()
main = hspec $ do
  describe "part 1" Part1Spec.spec
  describe "part 2" Part2Spec.spec
  describe "part 3" Part3Spec.spec
