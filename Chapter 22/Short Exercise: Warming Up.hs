-- Short Exercise: Warming Up

import Data.Char

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = cap . rev

fmapped :: [Char] -> [Char]
fmapped = cap <$> rev

-- Got a little help here because I was using xs in the monadic stucture making
-- my types come out wrong:
-- https://github.com/MattMSumner/haskell-progamming/blob/master/chapter22/short_exercises.hs
tupled :: [Char] -> ([Char], [Char])
tupled xs = (do
  a <- rev
  b <- cap
  return (a, b)) xs

tupled' :: [Char] -> ([Char], [Char])
tupled' xs = (rev >>= (\a ->
              cap >>= (\b ->
              return (a, b)))) xs
