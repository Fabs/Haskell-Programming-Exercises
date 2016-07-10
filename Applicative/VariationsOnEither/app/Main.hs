module Main where

import Lib

import Text.Printf (printf)

main :: IO ()
main = do
  putStr "Verifying Sum is a valid Functor and Applicative"
  verifySumIsFunctor
  verifySumIsApplicative
  putStr "\nVerifying Validation is a valid Functor and Applicative"
  verifyValidationIsFunctor
  verifyValidationIsApplicative
