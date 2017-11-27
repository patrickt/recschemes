module Dyna (main) where

import Part4 (change)
import Part5 (change')

import Criterion.Main

main = defaultMain
       [ bgroup "change (histo)" [ bench "10" $ whnf change 10
                                 , bench "35" $ whnf change 35
                                 , bench "95" $ whnf change 95
                                 ]
       , bgroup "change (dyna)"  [ bench "10" $ whnf change' 10
                                 , bench "35" $ whnf change' 35
                                 , bench "95" $ whnf change' 95
                                 ]
       ]
