module Cipher where

{--
Example usage

λ unCeaser 1 $ ceaser 1 "Hello"
"Hello"

--}


import Data.Char

ceaser :: Int    -- Number of right shifts
       -> String -- Original message
       -> String -- Encoded message
ceaser = map . ceaserShift

{-
Detect what type of character we have and right shift accordingly.
0-9
a-z
A-Z
-}
ceaserShift :: Int -> Char -> Char
ceaserShift i c
  | elem c ['0'..'9'] = shift (ord '0') (length ['0'..'9']) i c
  | elem c ['a'..'z'] = shift (ord 'a') (length ['a'..'z']) i c
  | elem c ['A'..'Z'] = shift (ord 'A') (length ['A'..'Z']) i c
  | otherwise         = c
  where
    shift :: Int -- Base Int
          -> Int -- Length of character set
          -> Int -- Right shift amount
          -> Char -- Source character to shift
          -> Char -- Output character
    shift b l i' c' = chr (b + (((ord c') - b) + (i' `mod` l)) `mod` l)

-- Use the same shift Int as used with ceaser to decode the message
unCeaser :: Int    -- Number of left shifts
         -> String -- Encoded message
         -> String -- Original message
unCeaser i = ceaser (-i)
