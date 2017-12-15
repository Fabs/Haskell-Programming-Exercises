module Lib where

import Data.Char

half x = x / 2

square x = x * x

twice f = f . f
fourTimes = twice . twice

-- Copied from Chapter 11
capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (x:xs) = (toUpper x):xs
