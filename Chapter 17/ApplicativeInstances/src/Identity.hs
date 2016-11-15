module Identity where
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure  = Identity
  Identity f <*> Identity a = Identity (f a)

-- QuickCheck arbitrary
instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> do arbitrary

-- Required for checkers
instance (Eq a) => EqProp (Identity a) where (=-=) = eq

-- Use checkers to verify the Applicative and Functor for Identity are valid
verifyIdentityIsFunctor :: IO ()
verifyIdentityIsFunctor = quickBatch $ functor (undefined :: Identity (Int,Int,Int))
verifyIdentityIsApplicative :: IO ()
verifyIdentityIsApplicative = quickBatch $ applicative (undefined :: Identity (Int,Int,Int))
