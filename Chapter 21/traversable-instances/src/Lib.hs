{-# LANGUAGE FlexibleContexts #-}

module Lib where

-- Chapter Exercises

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Data.Monoid

-- Traversable instances

-- Identity

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Foldable Identity where
  foldr f i (Identity a) = f a i

instance Applicative Identity where
  pure a = Identity a
  (<*>) (Identity f) fa = fmap f fa

instance Traversable Identity where
  traverse f (Identity a) = Identity <$> f a 

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

-- Required for checkers
instance (Eq a) => EqProp (Identity a) where (=-=) = eq

test_Identity :: IO ()
test_Identity = quickBatch (traversable (undefined :: Identity (Int, Int, [Int])))

-- Constant

newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Foldable (Constant a) where
  foldr _ i _ = i

--instance Applicative Identity where
--  pure a = Identity a
--  (<*>) (Identity f) fa = fmap f fa

instance Traversable (Constant a) where
  traverse _ (Constant a) = pure $ Constant a 

instance (Arbitrary a) => Arbitrary (Constant a b) where
  arbitrary = do
    a <- arbitrary
    return $ Constant a

-- Required for checkers
instance (Eq a) => EqProp (Constant a b) where (=-=) = eq

test_Constant :: IO ()
test_Constant = do
  let trigger :: Constant Int (Int, Int, [Int])
      trigger = undefined
  quickBatch (functor trigger)
  quickBatch (traversable trigger)

-- Maybe
data Optional a = Nada | Yep a deriving (Show, Eq)

instance Functor Optional where
  fmap _ Nada     = Nada
  fmap f (Yep a)  = Yep (f a)

instance Foldable Optional where
  foldr _ i Nada    = i
  foldr f i (Yep a) = f a i

instance Traversable Optional where
  traverse _ Nada    = pure Nada
  traverse f (Yep a) = Yep <$> f a

instance (Arbitrary a) => Arbitrary (Optional a) where
  arbitrary = do
    a <- arbitrary
    e <- elements [Nada, Yep a]
    return e

-- Required for checkers
instance (Eq a) => EqProp (Optional a) where (=-=) = eq

test_Optional :: IO ()
test_Optional = do
  let trigger :: Optional (Int, Int, [Int])
      trigger = undefined
  quickBatch (functor trigger)
  quickBatch (traversable trigger)

-- List
data List a = Nil | Cons a (List a) deriving (Show, Eq)

instance Monoid (List a) where
  mempty = Nil
  mappend Nil a = a
  mappend a Nil = a
  mappend (Cons a as) bs = Cons a (as <> bs)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a as) = Cons (f a) (fmap f as)

-- Got a hint from: https://github.com/MattMSumner/haskell-progamming/blob/master/chapter21/traversable.hs
-- foldMap needs to be implemented because it correctly uses monoid for Nil cases
instance Foldable List where
  foldMap _ Nil = mempty
  foldMap f (Cons a as) = (f a) <> foldMap f as

instance Applicative List where
  pure a = Cons a Nil
  (<*>) Nil _                   = Nil
  (<*>) _ Nil                   = Nil
  (<*>) (Cons f fs) (Cons a as) = Cons (f a) (f <$> as) <> (fs <*> (Cons a as))

instance Traversable List where
  sequenceA (Cons fa fas) = Cons <$> fa <*> (sequenceA fas)
  sequenceA Nil           = pure Nil

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    as <- arbitrary
    e <- elements [Cons a as, Nil]
    return e

-- Required for checkers
instance (Eq a) => EqProp (List a) where (=-=) = eq

test_List :: IO ()
test_List = do
  let trigger :: List (Int, Int, [Int])
      trigger = undefined
  quickBatch (functor trigger)
  quickBatch (traversable trigger)

-- Three
data Three a b c = Three a b c deriving (Show, Eq)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance Foldable (Three a b) where
  foldr f i (Three _ _ c) = f c i

instance Traversable (Three a b) where
  sequenceA (Three a b (fc)) = fmap (Three a b) fc

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

-- Required for checkers
instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where (=-=) = eq

test_Three :: IO ()
test_Three = do
  let trigger :: Three Int Int (Int, Int, [Int])
      trigger = undefined
  quickBatch (functor trigger)
  quickBatch (traversable trigger)

-- Pair
data Pair a b = Pair a b deriving (Show, Eq)

instance Functor (Pair a) where
  fmap f (Pair a b) = Pair a (f b)

instance Foldable (Pair a) where
  foldr f i (Pair _ b) = f b i

instance Traversable (Pair a) where
  sequenceA (Pair a (fb)) = fmap (Pair a) fb

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Pair a b

-- Required for checkers
instance (Eq a, Eq b) => EqProp (Pair a b) where (=-=) = eq

test_Pair :: IO ()
test_Pair = do
  let trigger :: Pair Int (Int, Int, [Int])
      trigger = undefined
  quickBatch (traversable trigger)

-- Big
data Big a b = Big a b b deriving (Show, Eq)

instance Functor (Big a) where
  fmap f (Big a b1 b2)  = Big a (f b1) (f b2)

instance Foldable (Big a) where
  foldMap f (Big _ b1 b2) = (f b1) <> (f b2)

instance Traversable (Big a) where
  sequenceA (Big a fb1 fb2) = Big a <$> fb1 <*> fb2

instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
  arbitrary = do
    a <- arbitrary
    b1 <- arbitrary
    b2 <- arbitrary
    return $ Big a b1 b2

-- Required for checkers
instance (Eq a, Eq b) => EqProp (Big a b) where (=-=) = eq

test_Big :: IO ()
test_Big = do
  let trigger :: Big Int (Int, Int, [Int])
      trigger = undefined
  quickBatch (traversable trigger)

-- Bigger
data Bigger a b = Bigger a b b b deriving (Show, Eq)

instance Functor (Bigger a) where
  fmap f (Bigger a b1 b2 b3)  = Bigger a (f b1) (f b2) (f b3)

instance Foldable (Bigger a) where
  foldMap f (Bigger _ b1 b2 b3) = (f b1) <> (f b2) <> (f b3)

instance Traversable (Bigger a) where
  sequenceA (Bigger a fb1 fb2 fb3) = Bigger a <$> fb1 <*> fb2 <*> fb3

instance (Arbitrary a, Arbitrary b) => Arbitrary (Bigger a b) where
  arbitrary = do
    a <- arbitrary
    b1 <- arbitrary
    b2 <- arbitrary
    b3 <- arbitrary 
    return $ Bigger a b1 b2 b3

-- Required for checkers
instance (Eq a, Eq b) => EqProp (Bigger a b) where (=-=) = eq

test_Bigger :: IO ()
test_Bigger = do
  let trigger :: Bigger Int (Int, Int, [Int])
      trigger = undefined
  quickBatch (traversable trigger)

-- S
data S n a = S (n a) a deriving (Eq, Show)

instance (Functor n) => Functor (S n) where
  fmap f (S na a) = S (fmap f na) (f a)

instance (Foldable n) => Foldable (S n) where
  foldMap f (S na a) = (foldMap f na) <> (f a)

instance Traversable n => Traversable (S n) where
  traverse f (S na a) = S <$> (traverse f na) <*> f a

instance (Functor n, Arbitrary (n a), Arbitrary a) => Arbitrary (S n a) where
  arbitrary = S <$> arbitrary <*> arbitrary

-- Required for checkers
instance (Applicative n, Testable (n Property), EqProp a) => EqProp (S n a) where
  (S x y) =-= (S p q) = (property $ (=-=) <$> x <*> p) .&. (y =-= q)

test_S :: IO ()
test_S = do
  --sample (arbitrary :: Gen (S [] Int))
  let trigger :: S [] (Int, Int, [Int])
      trigger = undefined
  -- Is not a valid Functor or Traversable!
  -- Is it supposed to be?
  quickBatch (functor trigger)
  quickBatch (traversable trigger)

-- Tree
data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a) deriving (Eq, Show)

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Node l a r) = Node (f <$> l) (f a) (f <$> r)

instance Foldable Tree where
  foldMap _ Empty    = mempty 
  foldMap f (Leaf a) = f a
  foldMap f (Node l a r) = (foldMap f l) <> (f a) <> (foldMap f r)

instance Traversable Tree where
  traverse _ Empty    = pure Empty
  traverse f (Leaf a) = Leaf <$> (f a)
  traverse f (Node l a r) = Node <$> (traverse f l) <*> (f a) <*> (traverse f r)

instance (Arbitrary a) => Arbitrary (Tree a) where
  arbitrary = do
    a <- arbitrary
    l <- arbitrary
    r <- arbitrary
    e <- elements [Empty, Leaf a, Node l a r]
    return e 

-- Required for checkers
instance (Eq a) => EqProp (Tree a) where (=-=) = eq

test_Tree :: IO ()
test_Tree = do
  --sample (arbitrary :: Gen (Tree [] Int))
  let trigger :: Tree (Int, Int, [Int])
      trigger = undefined
  quickBatch (functor trigger)
  quickBatch (traversable trigger)

