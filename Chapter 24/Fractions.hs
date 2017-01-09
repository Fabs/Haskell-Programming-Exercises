{- LANGUAGE OverloadedStrings #-}

module Text.Fractions where

import Control.Applicative
import Data.Ratio ((%))
import Text.Trifecta

badFraction = "1/0"
alsoBad = "10"
shouldWork = "1/2"
shouldAlsoWork = "2/1"

parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
--  [2]          [1]
  char '/'
--  [3]
  denominator <- decimal
--           [4]
  return (numerator % denominator)
--   [5]          [6]

virtuousFraction :: Parser Rational
virtuousFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator cannot be zero"
    _ -> return (numerator % denominator)

main :: IO ()
main = do
  print $ parseString parseFraction mempty badFraction
  print $ parseString parseFraction mempty shouldWork
  print $ parseString parseFraction mempty shouldAlsoWork
  print $ parseString parseFraction mempty alsoBad

testVirtuous :: IO ()
testVirtuous = do
  print $ parseString virtuousFraction mempty badFraction
  print $ parseString virtuousFraction mempty alsoBad
  print $ parseString virtuousFraction mempty shouldWork
  print $ parseString virtuousFraction mempty shouldAlsoWork

-- Exercise: Try Try
parseFractionalDecimal :: Parser Rational
parseFractionalDecimal = do
  whole_part <- decimal
  char '.'
  fraction_part <- decimal
  return ((toRational whole_part) + (fraction_part % (10 ^ (length (show fraction_part)) )))

parseFractional :: Parser Rational
parseFractional = (try virtuousFraction) <|> (try parseFractionalDecimal)
