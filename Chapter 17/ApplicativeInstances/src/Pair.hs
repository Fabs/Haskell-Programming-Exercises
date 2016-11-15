module Pair where
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
  pure a = Pair a a
  Pair f1 f2 <*> Pair x y = Pair (f1 x) (f2 y)

-- QuickCheck arbitrary
instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Pair x y

-- Required for checkers
instance (Eq a) => EqProp (Pair a) where (=-=) = eq

-- Use checkers to verify the Applicative and Functor for Identity are valid
verifyPairIsFunctor :: IO ()
verifyPairIsFunctor = quickBatch $ functor (undefined :: Pair (Int,Int,Int))
verifyPairIsApplicative :: IO ()
verifyPairIsApplicative = quickBatch $ applicative (undefined :: Pair (Int,Int,Int))
