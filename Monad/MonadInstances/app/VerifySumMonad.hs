module Main where

import Sum

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

main :: IO ()
main = do
  let a = (undefined :: Sum (Int, Int, Int) (Int, Int, Int))
  quickBatch $ functor a
  quickBatch $ applicative a
  quickBatch $ monad a
