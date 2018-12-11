{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module Part1 where

-- Check Part1Spec.hs for test cases that verify correctness.

import           Control.Arrow

data Lit
  = StrLit String
  | IntLit Int
  | Ident String
  deriving (Show, Eq)

data Expr
  = Index Expr Expr
  | Call Expr [Expr]
  | Unary String Expr
  | Binary Expr String Expr
  | Paren Expr
  | Literal Lit
  deriving (Show, Eq)

data Stmt
  = Break
  | Continue
  | Empty
  | IfElse Expr [Stmt] [Stmt]
  | Return (Maybe Expr)
  | While Expr [Stmt]
  | Expression Expr
  deriving (Show, Eq)

-- this would turn the expression
--    (((anArray[(10)])))
-- into
--    anArray[10]

flatten :: Expr -> Expr
-- base case: do nothing to literals
flatten (Literal i) = Literal i

-- this is the important case: we shed the Paren constructor and just
-- apply `flatten` to its contents
flatten (Paren e) = flatten e

-- all the other cases preserve their constructors and just apply
-- the flatten function to their children that are of type `Expr`.
flatten (Index e i)     = Index (flatten e) (flatten i)
flatten (Call e args)   = Call (flatten e) (map flatten args)
flatten (Unary op arg)  = Unary op (flatten arg)
flatten (Binary l op r) = Binary (flatten l) op (flatten r)

applyExpr :: (Expr -> Expr) -> Expr -> Expr
-- base case: applyExpr is the identity function on constants
applyExpr f (Literal i) = Literal i

-- recursive cases: apply f to each subexpression
applyExpr f (Paren p) = Paren (f p)
applyExpr f (Index e i) = Index (f e) (f i)
applyExpr f (Call e args) = Call (f e) (map f args)
applyExpr f (Unary op arg) = Unary op (f arg)
applyExpr f (Binary l op r) = Binary (f l) op (f r)

flatten' :: Expr -> Expr
flatten' (Paren e) = flatten e
flatten' x = applyExpr flatten x

data ExprF a
  = IndexF a a
  | CallF [a]
  | UnaryF String a
  | BinaryF a String a
  | ParenF a
  | LiteralF Lit
  deriving (Show, Eq, Functor) -- fmap for free

type Expr' = Term ExprF

newtype Term f = In { out :: f (Term f) }

-- These instances are pretty sinful, but we'll use them for now
-- rather than complicating things with Eq1 and Show1.
deriving instance (Eq (f (Term f))) => Eq (Term f)
deriving instance (Show (f (Term f))) => Show (Term f)

bottomUp :: Functor a => (Term a -> Term a) -> Term a -> Term a
bottomUp fn =
  out                    -- 1) unpack
  >>> fmap (bottomUp fn) -- 2) recurse
  >>> In                 -- 3) repack
  >>> fn                 -- 4) apply

flattenTerm :: Expr' -> Expr'
flattenTerm (In (ParenF e)) = e  -- remove all Parens
flattenTerm other = other       -- do nothing otherwise

flatten'' :: Expr' -> Expr'
flatten'' = bottomUp flattenTerm
