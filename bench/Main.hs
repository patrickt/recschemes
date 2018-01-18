module Main (main) where

import           Part4                 (change, coins)

import qualified Data.Functor.Foldable as RS

import           Criterion.Main
import           Control.Comonad.Cofree
import           Data.List             (partition)
import           Numeric.Natural

-- coins' :: [Natural]
-- coins' = fmap fromIntegral coins

-- kHistoChange :: Natural -> Int
-- kHistoChange = RS.histo go where
--   go :: Maybe (Cofree Maybe Int) -> Int
--   go Nothing = 1
--   go (Just (given :< attr)) = let
--     validCoins          = filter (<= (fromIntegral given)) coins'
--     remaining           = map ((fromIntegral given) -) validCoins
--     (zeroes, toProcess) = partition (== 0) remaining
--     results             = sum (map (_lookup attr) toProcess)
--     in length zeroes + toProcess

main :: IO ()
main = defaultMain
       [ bgroup "change (histo)" [ bench "10" $ whnf change 10
                                 , bench "35" $ whnf change 35
                                 , bench "95" $ whnf change 95
                                 ]
       -- , bgroup "change (dyna)"  [ bench "10" $ whnf change' 10
       --                           , bench "35" $ whnf change' 35
       --                           , bench "95" $ whnf change' 95
       --                           ]
       -- , bgroup "change (kmett histo)"  [ bench "10" $ whnf kHistoChange 10
       --                                  , bench "35" $ whnf kHistoChange 35
       --                                  , bench "95" $ whnf kHistoChange 95
       --                           ]
       ]
