\begin{code}
{-# LANGUAGE ScopedTypeVariables #-}
module Part5Spec (spec) where

import Part4
import Test.Hspec
import Part5

spec :: Spec
spec = describe "RPN notation" $ do
  let str = "15 7 1 1 + - / 3 * 2 1 1 + + -"
  it "should work correctly" $ do
    rpn str `shouldBe` [5]
    safeRPN str `shouldBe` (Success [5])

  let synerror = "lol lol lol"
  let argerror = "1 +"
  it "should handle errors" $ do
    safeRPN synerror `shouldBe` ParseError "lol"
    safeRPN argerror `shouldBe` TooFewArguments [1]
\end{code}
