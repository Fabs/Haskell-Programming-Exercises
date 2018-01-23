module Cipher where

{--
Example usage

λ unCeaser 1 $ ceaser 1 "Hello"
"Hello"

--}

import Data.Char
import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))
import System.Exit (exitFailure)

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
unCeaser = ceaser . negate


-- Chapter 11 Vigenère cipher

{- Not really a very clever way of encoding and decoding but it works

Also, doesn't encode the same way as described in the exercise.

λ vEncode (VKeyword "ally") (VPlain "meetatdawn")
VCipher "fiiktxhrpr"

λ vDecode (VKeyword "ally") (VCipher "fiiktxhrpr")
VPlain "meetatdawn"

-}

newtype VKeyword  = VKeyword String deriving Show
newtype VPlain    = VPlain  { unVPlain :: String } deriving (Show, Eq)
newtype VCipher   = VCipher { unVCipher :: String } deriving Show

vEncode :: VKeyword -> VPlain -> VCipher
vEncode (VKeyword []) (VPlain plain)  = VCipher plain
vEncode (VKeyword key) (VPlain plain) = VCipher (vEncode' key plain)
  where
    vEncode' _      []     = []
    vEncode' []     plain' = vEncode' key plain'
    vEncode' (k:ks) (p:ps) = (ceaserShift (ord k) p):(vEncode' ks ps)

vDecode:: VKeyword -> VCipher -> VPlain
vDecode (VKeyword []) (VCipher cipher)  = VPlain cipher
vDecode (VKeyword key) (VCipher cipher) = VPlain (vEncode' key cipher)
  where
    vEncode' _      []     = []
    vEncode' []     cipher' = vEncode' key cipher'
    vEncode' (k:ks) (c:cs) = (ceaserShift (negate $ ord k) c):(vEncode' ks cs)

-- Chapter 13 exercise
ceaserMain :: IO ()
ceaserMain = do
  hSetBuffering stdout NoBuffering
  putStr $ "Enter shift number for ceaser cipher: "
  shift <- getLine
  if (any (not . isDigit) shift)
  then do
    putStrLn "Sorry, you didn't enter an Int datatype. Exiting."
    exitFailure
  else
    return ()

  putStr $ "Enter string to encode: "
  srcString <- getLine

  putStrLn $ "The resulting cipher is: " ++ ceaser (read shift) srcString

vEncodeMain :: IO ()
vEncodeMain = do
  hSetBuffering stdout NoBuffering
  putStr $ "Enter keyword for vigenère cipher: "
  keyword <- getLine
  putStr $ "Enter string to encode: "
  srcString <- getLine

  putStrLn $ "The resulting cipher is: " ++ (show $ vEncode (VKeyword keyword) (VPlain srcString))
