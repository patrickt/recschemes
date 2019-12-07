{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE OverloadedStrings #-}

module Part2
  ( Expr (..)
  , Algebra
  , Coalgebra
  , call
  , cata
  , countNodes
  , prettyPrint
  , ana
  )
  where

import           Part1 (Term (..))
import           Control.Arrow    hiding (left, right)
import           Data.Monoid
import           Text.PrettyPrint (Doc)
import qualified Text.PrettyPrint as P

data Expr a
  = Literal { intVal :: Int }
  | Ident   { name :: String  }
  | Index   { target :: a, idx :: a }
  | Unary   { op :: String, target :: a }
  | Binary  { lhs :: a, op :: String, rhs :: a }
  | Call    { func :: a, args :: [a] }
  | Paren   { target :: a }
  deriving (Show, Eq, Functor)

ten, add, call :: Term Expr
ten  = In (Literal { intVal = 10 })
add  = In (Ident { name = "add" })
call = In (Call { func = add, args = [ten, ten]})---add(10, 10)

countNodes :: Expr Int -> Int

countNodes (Unary _ arg)         = arg + 1
countNodes (Binary left _ right) = left + right + 1
countNodes (Call fn args)        = fn + sum args + 1
countNodes (Index it idx)        = it + idx + 1
countNodes (Paren arg)           = arg + 1

type Algebra f a = f a -> a

cata :: (Functor f) => Algebra f a -> Term f -> a
cata f = out >>> fmap (cata f) >>> f

prettyPrint :: Expr Doc -> Doc

prettyPrint (Literal i) = P.int i
prettyPrint (Ident s) = P.text s

prettyPrint (Call f as)     = f <> P.parens (mconcat (P.punctuate "," as))  ---f(a,b...)
prettyPrint (Index it idx)  = it <> P.brackets idx                ---a[b]
prettyPrint (Unary op it)   = (P.text op) <> it                   ---op x
prettyPrint (Binary l op r) = l <> (P.text op) <> r               ---lhs op rhs
prettyPrint (Paren exp)     = P.parens exp                        ---(op)

type Coalgebra f a = a -> f a

ana :: (Functor f) => Coalgebra f a -> a -> Term f
ana f = In <<< fmap (ana f) <<< f

nested :: Int -> Term Expr
nested n = ana go n where
  go :: Coalgebra Expr Int
  go 0 = Literal n
  go n = Paren (n - 1)
