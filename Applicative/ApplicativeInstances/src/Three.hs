module Three where
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where fmap f (Three x y z) = Three x y (f z)

instance (Monoid m1, Monoid m2) => Applicative (Three m1 m2) where
  pure a = Three mempty mempty a
  Three mx1 my1 f <*> Three mx2 my2 z = Three (mx1 `mappend` mx2) (my1 `mappend` my2) (f z)

-- QuickCheck arbitrary
instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Three x y z

-- Required for checkers
instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where (=-=) = eq

-- Use checkers to verify the Applicative and Functor for Identity are valid
verifyThreeIsFunctor :: IO ()
verifyThreeIsFunctor = quickBatch $ functor (undefined :: Three (Int,Int,Int) (Int,Int,Int) (Int,Int,Int))
verifyThreeIsApplicative :: IO ()
verifyThreeIsApplicative = quickBatch $ applicative (undefined :: Three (String,String,String) (String,String,String) (Int, Int, Int))
