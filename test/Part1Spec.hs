{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes  #-}

module Part1Spec (spec) where

import           Part1
import           Test.Hspec

-- These instances are pretty sinful, but we'll use them for now
-- rather than complicating things with Eq1 and Show1.
deriving instance (Eq (f (Term f))) => Eq (Term f)
deriving instance (Show (f (Term f))) => Show (Term f)

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
        index a b = In (Index' a b)
        paren a = In (Paren' a)
        lit i = In (Literal' (IntLit i))
        name n = In (Literal' (Ident n))
        nested' = index (paren (paren (paren (name "anArray")))) (paren (lit 10))
        flattened' = index (name "anArray") (lit 10)
      in flatten'' nested' `shouldBe` flattened'
