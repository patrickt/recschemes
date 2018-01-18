\begin{code}
{-# LANGUAGE ScopedTypeVariables #-}
module Part5Spec (spec) where

import Part4
import Test.Hspec
import Part5

spec :: Spec
spec = describe "RPN notation" $ do
  let str = "15 7 1 1 + - / 3 * 2 1 1 + + -"
  it "should work correctly" $
    rpn str `shouldBe` [5]
\end{code}
