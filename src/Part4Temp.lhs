\begin{code}
module Part4
  ( change )
  where

import           Part2
import           Prelude                hiding (lookup)

import           Control.Arrow          hiding (left, right)
import           Data.List              hiding (lookup)
import qualified System.Random          as Random
import           Text.PrettyPrint.Boxes

-- These instances are pretty sinful, but we'll use them for now
-- rather than complicating things with Eq1 and Show1.
deriving instance (Eq (f (Term f))) => Eq (Term f)
deriving instance (Show (f (Term f))) => Show (Term f)

data Attr f a = Attr
              { attribute :: a
              , hole      :: f (Attr f a)
              }

type CVAlgebra f a = f (Attr f a) -> a

histo :: Functor f => CVAlgebra f a -> Term f -> a
histo h = attribute . worker where
    worker = out >>> fmap worker >>> (h &&& id) >>> uncurry Attr

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
  = Continue a
  | Defer (f (CoAttr f a))

type CVCoalgebra f a = a -> f (CoAttr f a)

futu :: Functor f => CVCoalgebra f a -> a -> Term f
futu f = In <<< fmap worker <<< f where
  worker (Continue a) = futu f a
  worker (Defer g)    = In (fmap worker g)

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

split' :: Seed -> (Seed, Seed)
split' (Seed h rand) = (Seed h left, Seed h right)
  where (left, right) = Random.split rand

grow :: Seed -> (Action, Seed, Seed)
grow seed@(Seed h rand) = (choose choice, left { height = h + 1}, right { height = h + 1})
  where (choice, _) = Random.randomR (1 :: Int, 5) rand
        (left, right) = split' seed
        choose 1 = Flower
        choose 2 = Branch
        choose _ = Upwards


sow :: CVCoalgebra Plant Seed
sow seed =
  let (action, left, right) = grow seed
  in case (action, height seed) of
    (_, 0)       -> Root (Continue left)
    (_, 10)      -> Bloom
    (Flower, _)  -> Bloom
    (Upwards, _) -> Stalk (Continue right)
    (Branch, _)  -> Fork  (Defer (Stalk (Continue left)))
                         (Defer Bloom)
                         (Defer (Stalk (Continue right)))

render :: Algebra Plant Box
render Bloom    = "8"
render (Root a) = vcat center1 ["X", a]
\end{code}
