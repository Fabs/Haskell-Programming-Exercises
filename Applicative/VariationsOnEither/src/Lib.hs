module Lib
    ( verifySumIsFunctor
    , verifySumIsApplicative
    , verifyValidationIsFunctor
    , verifyValidationIsApplicative
    ) where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

-- Arbitrary instance for Sum
instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = do 
    first <- arbitrary
    second <- arbitrary
    list <- elements [First first, Second second]
    return $ list


instance Functor (Sum a) where
  fmap _ (First x) = First x
  fmap f (Second x) = Second (f x)

-- Required for checkers
instance (Eq a, Eq b) => EqProp (Sum a b) where (=-=) = eq

-- Use checkers to verify the Fuctor for Sum is valid
verifySumIsFunctor :: IO ()
verifySumIsFunctor = quickBatch $ functor (undefined::Sum (Int, Int, Int) (Int, Int, Int))

instance Applicative (Sum a) where
  pure  = Second
  (<*>) (Second f) (Second x) = Second (f x)
  (<*>) (First x) _           = First x
  (<*>) _ (First x)           = First x

-- Use checkers to verify the Fuctor for Sum is valid
verifySumIsApplicative :: IO ()
verifySumIsApplicative = quickBatch $ applicative (undefined::Sum (Int, Int, Int) (Int, Int, Int))

data Validation e a =
    Error e
  | Success a
  deriving (Eq, Show)

-- Arbitrary instance for Validation
instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = do 
    e <- arbitrary
    s <- arbitrary
    list <- elements [Error e, Lib.Success s]
    return $ list

-- same as Sum/Either
instance Functor (Validation e) where
  fmap _ (Error x) = Error x
  fmap f (Lib.Success x) = Lib.Success (f x)

-- Required for checkers
instance (Eq a, Eq b) => EqProp (Validation a b) where (=-=) = eq

-- Use checkers to verify the Functor for Validation is valid
verifyValidationIsFunctor :: IO ()
verifyValidationIsFunctor = quickBatch $ functor (undefined::Validation (Int, Int, Int) (Int, Int, Int))

instance Monoid e => Applicative (Validation e) where
  pure  = Lib.Success
  (<*>) (Lib.Success f) (Lib.Success x)   = Lib.Success $ f x
  (<*>) (Error e1) (Error e2)   = Error (e1 `mappend` e2)
  (<*>) (Error e) _             = Error e
  (<*>) _ (Error e)             = Error e

-- Use checkers to verify the Applicative for Validation is valid
verifyValidationIsApplicative :: IO ()
verifyValidationIsApplicative = quickBatch $ applicative (undefined::Validation (String, String, String) (String, String, String))
