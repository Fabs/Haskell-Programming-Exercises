-- | A library to do stuff.
module Lib
    (
      --ourAdd
    ) where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

-- Functor instance
instance Functor List where
  fmap _ Nil        = Nil
  fmap f (Cons a l) = Cons (f a) (fmap f l)

-- Required for QuickCheck arbitrary
instance (Eq a) => EqProp (List a) where (=-=) = eq

-- Functor QuickCheck arbitrary
instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do 
    a' <- arbitrary
    ls <- arbitrary
    list <- elements [Cons a' (ls), Nil]
    return $ list

verifyListIsFunctor :: IO ()
verifyListIsFunctor = quickBatch $ functor (Cons ("1", (1::Integer), True) Nil)

instance Applicative List where
  pure  = undefined
  (<*>) = undefined
