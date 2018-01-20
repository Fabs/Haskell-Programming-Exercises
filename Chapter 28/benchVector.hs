module Main where

import Criterion.Main
import qualified Data.Vector as VB
import qualified Data.Vector.Unboxed as VU

vb :: VB.Vector Int
vb = VB.fromList $ take 100000 stream
  where stream = iterate (+2) 0

vu :: VU.Vector Int
vu = VU.fromList $ take 100000 stream
  where stream = iterate (+2) 0

dropsVectorBoxed :: Int -> VB.Vector Int
dropsVectorBoxed i = VB.drop i vb

dropsVectorUnboxed :: Int -> VU.Vector Int
dropsVectorUnboxed i = VU.drop i vu

main :: IO ()
main = defaultMain
  [ bench "drops vector boxed 9970" $ whnf dropsVectorBoxed 9970
  , bench "drops vector unboxed 9970" $ whnf dropsVectorUnboxed 9970
  ]

{-

After installing profiling libraries with `stack install --library-profiling criterion`
I was able to profile this main function using:

stack ghc -- -prof -fprof-auto -rtsopts -O2 benchVector.hs
./benchVector +RTS -P
cat benchVector.prof | grep benchVector

vb resulted in 18400168 bytes
vu resulted in 15200112 bytes

according to the profiler.

-}
