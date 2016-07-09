-- | A library to do stuff.
module Lib
    (
      verifyListIsApplicative
    , verifyListIsFunctor
    ) where

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

-- Use checkers to verify the Fuctor for List is valid
verifyListIsFunctor :: IO ()
verifyListIsFunctor = quickBatch $ functor (Cons ("1", (1::Integer), True) Nil)

-- Applicative instance for List
instance Applicative List where
  pure a = Cons a Nil
  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  (<*>) a b = flatMap (\x -> x <$> b) a where
    append :: List a -> List a -> List a
    append Nil ys = ys
    append (Cons x xs) ys = Cons x $ xs `append` ys
    fold :: (a -> b -> b) -> b -> List a -> b
    fold _ b' Nil = b'
    fold f b' (Cons h t) = f h (fold f b' t)
    concat' :: List (List a) -> List a
    concat' = fold append Nil
    -- write this in terms of concat' and fmap
    flatMap :: (a' -> List b') -> List a' -> List b'
    flatMap f as = concat' $ f <$> as

-- Use checkers to verify the Applicative for List is valid
verifyListIsApplicative :: IO ()
verifyListIsApplicative = quickBatch $ applicative (Cons ("1", (1::Integer), True) Nil)

-- Required for QuickCheck arbitrary
instance (Eq a) => EqProp (List a) where (=-=) = eq

-- Functor QuickCheck arbitrary
instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do 
    a' <- arbitrary
    ls <- arbitrary
    list <- elements [Cons a' (ls), Nil]
    return $ list

