module Main where

import Lib

--import Text.Printf (printf)

main :: IO ()
main = do
  putStr "Verifying List is a valid Functor and Applicative using 'checkers' library"
  verifyListIsFunctor
  verifyListIsApplicative
