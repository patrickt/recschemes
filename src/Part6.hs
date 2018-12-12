{-# LANGUAGE DeriveFunctor, FlexibleInstances, InstanceSigs, MultiParamTypeClasses, ScopedTypeVariables, TypeFamilies #-}
module Part6 where

import Control.Comonad
import Control.Comonad.Cofree
import Control.Comonad.Env
import Control.Arrow
import Data.Functor.Foldable

newtype Ledger t f a = Ledger { getLedger :: EnvT t (Cofree f) a } deriving Functor

instance Functor f => Comonad (Ledger t f) where
  extract = getLedger >>> extract -- delegate to EnvT's extract
  duplicate l@(Ledger w) = Ledger (l <$ w) -- add a new Ledger layer to the input

instance Functor f => ComonadEnv t (Ledger t f) where
  ask :: Ledger t f a -> t
  ask = getLedger >>> ask -- delegate to EnvT, again

instance Functor f => ComonadCofree f (Ledger t f) where
  unwrap :: Ledger t f a -> f (Ledger t f a)
  unwrap = getLedger >>> unwrap >>> fmap Ledger -- delegate to EnvT+Cofree's unwrap

distLedger :: Comonad f => f (Ledger t f a) -> Ledger t f (f a)

distLedger f = Ledger (EnvT environ cofree) where
  environ = ask (extract f)
  cofree = fmap extract f :< fmap distInnards f
  distInnards (Ledger (EnvT _ (x :< y))) = distHisto y

distLedger' :: (Corecursive t, f ~ Base t) => f (Ledger t f a) -> Ledger t f (f a)
distLedger' f = Ledger (EnvT environ cofree) where
  environ = embed (fmap ask f)
  cofree = fmap extract f :< fmap distInnards f
  distInnards (Ledger (EnvT _ (x :< y))) = distHisto y

distLedger'' :: Corecursive t => Base t (Ledger t (Base t) a) -> Ledger t (Base t) (Base t a)
distLedger'' = fmap getLedger >>> distParaT distHisto >>> Ledger
