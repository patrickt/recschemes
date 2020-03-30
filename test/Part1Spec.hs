{-# LANGUAGE AllowAmbiguousTypes  #-}

module Part1Spec (spec) where

import           Part1
import           Test.Hspec

spec :: Spec
spec = do
  let nested = Index (Paren (Paren (Paren (Literal (Ident "anArray"))))) (Literal (IntLit 10))
  let flattened = Index (Literal (Ident "anArray")) (Literal (IntLit 10))

  describe "manual flatten" $
    it "should flatten (((anArray[(10)]))) into anArray[10]" $
      flatten nested `shouldBe` flattened

  describe "flatten with applyExpr" $
    it "should flatten (((anArray[(10)]))) into anArray[10]" $
      flatten' nested `shouldBe` flattened

  describe "flatten with bottomUp" $
    it "should also do the same thing" $
      let
        index a b = In (IndexF a b)
        paren a = In (ParenF a)
        lit i = In (LiteralF (IntLit i))
        name n = In (LiteralF (Ident n))
        nested' = index (paren (paren (paren (name "anArray")))) (paren (lit 10))
        flattened' = index (name "anArray") (lit 10)
      in flatten'' nested' `shouldBe` flattened'
