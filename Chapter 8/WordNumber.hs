module WordNumber where

import Data.List (intersperse)

-- Should return Nothing or not use Int because we cannot deal
-- with a integer not between -1 and 10
digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"
digitToWord _ = "#*$(#&$*(&*#($&"

-- Cannot deal with negatives in digitToWords so just return abs value
digits :: Int -> [Int]
digits num = digits' (abs num)
  where digits' n
           | n < 10 && n > -1 = [abs n]
           | otherwise        = ((digits . div n) 10) ++ [mod n 10]


wordNumber :: Int -> String
wordNumber n = (concat . intersperse "-" . map digitToWord) $ digits n
