module Main where

import Identity
import Pair
import Two
import Three
import Three_2
import Four
import Four_2
--import Text.Printf (printf)

main :: IO ()
main = do
  putStr "Verify Identity applicative"
  verifyIdentityIsFunctor
  verifyIdentityIsApplicative
  putStrLn ""

  putStr "Verify Pair applicative"
  verifyPairIsFunctor
  verifyPairIsApplicative
  putStrLn ""

  putStr "Verify Two applicative"
  verifyTwoIsFunctor
  verifyTwoIsApplicative
  putStrLn ""

  putStr "Verify Three applicative"
  verifyThreeIsFunctor
  verifyThreeIsApplicative
  putStrLn ""

  putStr "Verify Three' applicative"
  verifyThree'IsFunctor
  verifyThree'IsApplicative
  putStrLn ""

  putStr "Verify Four  applicative"
  verifyFourIsFunctor
  verifyFourIsApplicative
  putStrLn ""

  putStr "Verify Four' applicative"
  verifyFourIsFunctor
  verifyFourIsApplicative
  putStrLn ""
