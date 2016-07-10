module Four_2 where
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where fmap f (Four' w x y z) = Four' w x y (f z)

instance (Monoid m) => Applicative (Four' m) where
  pure a = Four' mempty mempty mempty a
  Four' mx1 my1 mz1 f  <*> Four' mx2 my2 mz2 x = Four' (mx1 `mappend` mx2) (my1 `mappend` my2) (mz1 `mappend` mz2) (f x)

-- QuickCheck arbitrary
instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    w <- arbitrary
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Four' w x y z

-- Required for checkers
instance (Eq a, Eq b) => EqProp (Four' a b) where (=-=) = eq

-- Use checkers to verify the Applicative and Functor are valid
verifyFour'IsFunctor :: IO ()
verifyFour'IsFunctor = quickBatch $ functor (undefined :: Four' (Int,Int,Int) (Int,Int,Int))
verifyFour'IsApplicative :: IO ()
verifyFour'IsApplicative = quickBatch $ applicative (undefined :: Four' (String,String,String) (String,String,String))
