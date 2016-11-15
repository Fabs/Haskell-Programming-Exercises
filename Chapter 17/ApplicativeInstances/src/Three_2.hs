module Three_2 where
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where fmap f (Three' x y z) = Three' x (f y) (f z)

instance (Monoid m) => Applicative (Three' m) where
  pure a = Three' mempty a a
  Three' mx1 f g  <*> Three' mx2 x y = Three' (mx1 `mappend` mx2) (f x) (g y)

-- QuickCheck arbitrary
instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Three' x y z

-- Required for checkers
instance (Eq a, Eq b) => EqProp (Three' a b) where (=-=) = eq

-- Use checkers to verify the Applicative and Functor are valid
verifyThree'IsFunctor :: IO ()
verifyThree'IsFunctor = quickBatch $ functor (undefined :: Three' (Int,Int,Int) (Int,Int,Int))
verifyThree'IsApplicative :: IO ()
verifyThree'IsApplicative = quickBatch $ applicative (undefined :: Three' (String,String,String) (Int, Int, Int))
