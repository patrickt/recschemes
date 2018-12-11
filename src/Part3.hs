{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}

module Part3
  ( para
  , para'
  , para''
  , apo
  , fastPretty
  , Term (..)
  , Expr (..)
  , RAlgebra
  , RCoalgebra
  ) where

import           Part1 (Term (..))
import           Control.Arrow
import           Data.Function
import           Data.Monoid
import           Data.String
import           Text.PrettyPrint (Doc)
import qualified Text.PrettyPrint as Pretty

type RAlgebra f a = f (Term f, a) -> a

para :: (Functor f) => RAlgebra f a -> Term f -> a
para rAlg = out >>> fmap fanout >>> rAlg
    where fanout t = (t, para rAlg t)

para' :: Functor f => RAlgebra f a -> Term f -> a
para' f = out >>> fmap (id &&& para' f) >>> f

type RAlgebra' f a = Term f -> f a -> a

-- The & function is reverse function application,
-- just like the $ operator, but with its arguments flipped.
para'' :: Functor f => RAlgebra' f a -> Term f -> a
para'' alg t = out t & fmap (para'' alg) & alg t

type Algebra f a = f a -> a

-- In retrospect, fastPretty is an ugly and contrived example.
-- I hope you will forgive me.

data Expr a
  = Literal { intVal :: Int }
  | Ident   { name :: String  }
  | Index   { target :: a, idx :: a }
  | Unary   { op :: String, target :: a }
  | Binary  { lhs :: a, op :: String, rhs :: a }
  | Call    { func :: a, args :: [a] }
  | Paren   { target :: a }
  deriving (Show, Eq, Functor)

instance IsString (Expr a) where fromString = Ident
instance IsString (Term Expr) where fromString = In . Ident

fastPretty :: RAlgebra' Expr Doc

-- All our cases, aside from the `Call` nodes in which
-- we are interested, are the same as in the pretty-printing
-- catamorphism in the previous installment. We just ignore
-- the first `Term` argument because it doesn't have anything we need
-- to look at.
fastPretty _ (Literal i) = Pretty.int i
fastPretty _ (Ident s)   = Pretty.text s
-- uninteresting cases omitted, blah blah blah

-- Here's where it gets interesting. We're going to look
-- at the first argument to determine  whether this is a
-- `Call` node with the function name (an `Ident`) named `id`.
-- If so, we'll just return the only argument provided.
fastPretty (In (Call { func = "id" }))
           (Call {args = [theArg]}) = theArg

-- Otherwise, we won't look at the first `Term` argument,
-- and just glom the name and the parenthesized and
-- comma-separated arguments together.
fastPretty _ (Call f args) = f <> Pretty.parens (mconcat ("," `Pretty.punctuate` args))

-- Straightforward ALGOL-style syntax for the remaining cases
fastPretty _ (Index it idx)  = it <> Pretty.brackets idx
fastPretty _ (Unary op it)   = Pretty.text op <> it
fastPretty _ (Binary l op r) = l <> Pretty.text op <> r
fastPretty _ (Paren ex)      = Pretty.parens ex

type RCoalgebra f a = a -> f (Either (Term f) a)

apo :: Functor f => RCoalgebra f a -> a -> Term f
apo f = In <<< fmap fanin <<< f where fanin = either id (apo f)
