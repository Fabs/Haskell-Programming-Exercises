module Four where
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where fmap f (Four w x y z) = Four w x y (f z)

instance (Monoid m1, Monoid m2, Monoid m3) => Applicative (Four m1 m2 m3) where
  pure a = Four mempty mempty mempty a
  Four mx1 my1 mz1 f  <*> Four mx2 my2 mz2 x = Four (mx1 `mappend` mx2) (my1 `mappend` my2) (mz1 `mappend` mz2) (f x)

-- QuickCheck arbitrary
instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    w <- arbitrary
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Four w x y z

-- Required for checkers
instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where (=-=) = eq

-- Use checkers to verify the Applicative and Functor are valid
verifyFourIsFunctor :: IO ()
verifyFourIsFunctor = quickBatch $ functor (undefined :: Four (Int,Int,Int) (Int,Int,Int) (Int, Int, Int) (Int, Int, Int))
verifyFourIsApplicative :: IO ()
verifyFourIsApplicative = quickBatch $ applicative (undefined :: Four (String,String,String) (String,String,String) (String,String,String) (Int, Int, Int))
