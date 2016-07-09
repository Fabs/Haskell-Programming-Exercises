-- | A library to do stuff.
module Lib
    (
      verifyListIsApplicative
    , verifyListIsFunctor
    , verifyZipList'IsFunctor
    , verifyZipList'IsApplicative
    , take'
    , List (Cons, Nil)
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
    fold :: (a -> b -> b) -> b -> List a -> b
    fold _ b' Nil = b'
    fold f b' (Cons h t) = f h (fold f b' t)
    concat' :: List (List a) -> List a
    concat' = fold append Nil
    -- write this in terms of concat' and fmap
    flatMap :: (a' -> List b') -> List a' -> List b'
    flatMap f as = concat' $ f <$> as

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

-- Use checkers to verify the Applicative for List is valid
verifyListIsApplicative :: IO ()
verifyListIsApplicative = quickBatch $ applicative (Cons ("1", (1::Integer), True) Nil)

take' :: Int -> List a -> List a
take' _ Nil   = Nil
take' i (Cons x xs)
  | i > 0     = (Cons x Nil) `append` (take' (i-1) xs)
  | otherwise = Nil

-- ZipList newtype
newtype ZipList' a =
  ZipList' (List a)
  deriving (Eq, Show)


instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

-- Use checkers to verify the Fuctor for ZipList' is valid
verifyZipList'IsFunctor :: IO ()
verifyZipList'IsFunctor = quickBatch $ functor (undefined :: ZipList' (String, String, String))

instance Applicative ZipList' where
  pure a                        = ZipList' $ Cons a (pure a)
  (<*>) _ (ZipList' Nil)        = ZipList' Nil
  (<*>) (ZipList' Nil) _        = ZipList' Nil
  (<*>) (ZipList' (Cons f fs)) (ZipList' (Cons x xs))
    = ZipList' $ Cons (f x) (fs <*> xs)

-- Use checkers to verify the Fuctor for ZipList' is valid
verifyZipList'IsApplicative :: IO ()
verifyZipList'IsApplicative = quickBatch $ applicative (undefined :: ZipList' (String, String, String))

-- Required for checkers
instance (Eq a) => EqProp (List a) where (=-=) = eq

-- QuickCheck arbitrary
instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do 
    a' <- arbitrary
    ls <- arbitrary
    list <- elements [Cons a' (ls), Nil]
    return $ list

-- Required for checkers
instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
                in take' 3000 l
          ys' = let (ZipList' l) = ys
                in take' 3000 l

-- QuickCheck arbitrary for ZipList'
instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> arbitrary
