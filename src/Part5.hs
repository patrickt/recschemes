{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE DeriveFunctor       #-}
module Part5
  ( Token (..)
  , List (..)
  , Result (..)
  , parseRPN
  , evalRPN
  , rpn
  , chrono
  , safeRPN
  ) where

import Part1 (Term (..))
import Part2
import Part3
import Part4

import Control.Arrow ((>>>), (<<<), (|||), (&&&))
import Data.Functor.Compose
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Char
import Text.Read (readMaybe)
import Data.Monoid
import Debug.Trace

hylo :: Functor f => Algebra f b -> Coalgebra f a -> a -> b
hylo alg coalg = ana coalg >>> cata alg

data Token
  = Lit Int
  | Op (Int -> Int -> Int)

parseToken :: String -> Token
parseToken "+" = Op (+)
parseToken "-" = Op (-)
parseToken "*" = Op (*)
parseToken "/" = Op div
parseToken num = Lit $ read num

data List a b
  = Cons a b
  | Nil
    deriving (Show, Eq, Functor)

parseRPN :: Coalgebra (List Token) String
parseRPN ""  = Nil
parseRPN str = Cons token newSeed
  where (x, rest) = span (not . isSpace) str
        token     = parseToken x
        newSeed   = dropWhile isSpace rest

type Stack = [Int]

evalRPN :: Algebra (List Token) (Stack -> Stack)
evalRPN Nil stack                      = stack
evalRPN (Cons (Lit i) cont) stack      = cont (i : stack)
evalRPN (Cons (Op fn) cont) (a:b:rest) = cont (fn b a : rest)
evalRPN _ stack                        = error ("too few arguments on stack: " <> show stack)

rpn :: String -> Stack
rpn s = hylo evalRPN parseRPN s []

hylo' :: Functor f => Algebra f b -> Coalgebra f a -> a -> b
hylo' alg coalg = coalg >>> fmap (hylo' alg coalg) >>> alg

chrono :: Functor f => CVAlgebra f b -> CVCoalgebra f a -> a -> b
chrono cvalg cvcoalg = futu cvcoalg >>> histo cvalg

elgot :: Functor f => Algebra f b -> (a -> Either b (f a)) -> a -> b
elgot alg coalg = coalg >>> (id ||| (fmap (elgot alg coalg) >>> alg))

data Result
  = Success Stack
  | ParseError String
  | TooFewArguments Stack
    deriving (Eq, Show)

type Cont = Result -> Result

safeToken :: String -> Either Cont Token
safeToken "+" = Right (Op (+))
safeToken "-" = Right (Op (-))
safeToken "*" = Right (Op (*))
safeToken "/" = Right (Op div)
safeToken str = case readMaybe str of
  Just num -> Right (Lit num)
  Nothing  -> Left  (const (ParseError str))

safeParse :: String -> Either Cont (List Token String)
safeParse ""  = return Nil
safeParse str = do
  let (x, rest) = span (not . isSpace) str
  let newSeed   = dropWhile isSpace rest
  parsed <- safeToken x
  return $ Cons parsed newSeed

safeEval :: Algebra (List Token) Cont
safeEval (Cons (Lit i) cont) (Success stack) = cont (Success (i : stack))
safeEval (Cons (Op fn) cont) (Success s)     = cont (case s of
  (a:b:rest) -> Success (fn b a : rest)
  _          -> TooFewArguments s)
safeEval _ result  = result

safeRPN :: String -> Result
safeRPN s = elgot safeEval safeParse s (Success [])

coelgot :: Functor f => ((a, f b) -> b) -> Coalgebra f a -> a -> b
coelgot alg coalg = alg <<< (id &&& (fmap (coelgot alg coalg) <<< coalg))
