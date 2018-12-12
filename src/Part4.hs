{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE OverloadedStrings    #-}

module Part4
  ( Attr (..)
  , Cent
  , CoAttr (..)
  , CVAlgebra
  , CVCoalgebra
  , Nat (..)
  , coins
  , change
  , compress
  , futu
  , histo
  ) where
import           Part1                  (Term (..))
import           Part2
import           Part3                  (RAlgebra, RCoalgebra)
import           Prelude                hiding (lookup)

import           Control.Arrow          hiding (left, right)
import           Data.List              hiding (lookup)
import qualified System.Random          as Random
import           Text.PrettyPrint.Boxes

data Attr f a = Attr
              { attribute :: a
              , hole      :: f (Attr f a)
              }

type CVAlgebra f a = f (Attr f a) -> a

histo :: Functor f => CVAlgebra f a -> Term f -> a
histo h = out >>> fmap worker >>> h where
    worker t = Attr (histo h t) (fmap worker (out t))

type Cent = Int

coins :: [Cent]
coins = [50, 25, 10, 5, 1]

data Nat a
    = Zero
    | Next a
    deriving Functor

-- Convert from a natural number to its foldable equivalent, and vice versa.
expand :: Int -> Term Nat
expand 0 = In Zero
expand n = In (Next (expand (n - 1)))

compress :: Nat (Attr Nat a) -> Int
compress Zero              = 0
compress (Next (Attr _ x)) = 1 + compress x

change :: Cent -> Int
change amt = histo go (expand amt) where
  go :: Nat (Attr Nat Int) -> Int
  go Zero = 1
  go curr@(Next attr) = let
    given               = compress curr
    validCoins          = filter (<= given) coins
    remaining           = map (given -) validCoins
    (zeroes, toProcess) = partition (== 0) remaining
    results             = sum (map (lookup attr) toProcess)
    in length zeroes + results

lookup :: Attr Nat a -> Int -> a
lookup cache 0 = attribute cache
lookup cache n = lookup inner (n - 1) where (Next inner) = hole cache

data CoAttr f a
  = Automatic a
  | Manual (f (CoAttr f a))

type CVCoalgebra f a = a -> f (CoAttr f a)

futu :: Functor f => CVCoalgebra f a -> a -> Term f
futu f = In <<< fmap worker <<< f where
    worker (Automatic a) = futu f a        -- continue through this level
    worker (Manual g) = In (fmap worker g) -- omit folding this level,
                                           -- delegating to the worker
                                           -- to perform any needed
                                           -- unfolds later on.

data Plant a
  = Root a     -- every plant starts here
  | Stalk a    -- and continues upwards
  | Fork a a a -- but can trifurcate at any moment
  | Bloom      -- eventually terminating in a flower
    deriving (Show, Functor)

data Action
  = Flower  -- stop growing now
  | Upwards -- grow up with a Stalk
  | Branch  -- grow up with a Fork

data Seed = Seed
    { height :: Int
    , rng    :: Random.StdGen
    }

grow :: Seed -> (Action, Seed, Seed)
grow seed@(Seed h rand) = (choose choice, left { height = h + 1}, right { height = h + 1})
  where (choice, _) = Random.randomR (1 :: Int, 5) rand
        (leftR, rightR) = Random.split rand
        left = Seed h leftR
        right = Seed h rightR
        choose 1 = Flower
        choose 2 = Branch
        choose _ = Upwards

sow :: CVCoalgebra Plant Seed

sow seed =
  let (action, left, right) = grow seed
  in case (action, height seed) of
    (_, 0)       -> Root (Automatic left)
    (_, 10)      -> Bloom
    (Flower, _)  -> Bloom
    (Upwards, _) -> Stalk (Automatic right)
    (Branch, _)  -> Fork (Manual (Stalk (Automatic left)))
                         (Manual Bloom)
                         (Manual (Stalk (Automatic right)))

-- I can't find the original implementation I had of this function. I will
-- do it more properly later.
render :: Algebra Plant Box
render _ = "TODO"
