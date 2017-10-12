module Part3Spec
    ( spec
    ) where

import           Part3
import           Test.Hspec

spec :: Spec
spec = describe "fastPretty" $
  it "should render id(x) as x" $ do
    let call = In (Call (In (Ident "id")) [In (Ident "x")])
    show (para'' fastPretty call) `shouldBe` "x"
