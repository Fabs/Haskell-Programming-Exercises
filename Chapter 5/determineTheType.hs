{-# LANGUAGE NoMonomorphismRestriction #-}

module DetermineTheType where

-- simple example
example = 1

_1a = (* 9) 6
_1b = head [(0,"doge"),(1,"kitteh")]
_1c = head [(0 :: Integer, "doge"),(1,"kitteh")]
_1d = if False then True else False
_1e = length [1,2,3,4,5]
_1f = (length [1,2,3,4]) > (length "TACOCAT")

_2 = w where
  x = 5
  y = x + 5
  w = y * 10

_3 = z where
  x = 5
  y = x + 5
  z y = y * 10

_4 = f where
  x = 5
  y = x + 5
  f = 4 / y

_5 = f where
  x = "Julie"
  y = " <3 "
  z = "Haskell"
  f = x ++ y ++ z
