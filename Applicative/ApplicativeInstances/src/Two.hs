module Two where
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where fmap f (Two x y) = Two x (f y)

instance (Monoid m) => Applicative (Two m) where
  pure a = Two mempty a
  Two m1 f2 <*> Two m2 y = Two (m1 `mappend` m2) (f2 y)

-- QuickCheck arbitrary
instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Two x y

-- Required for checkers
instance (Eq a, Eq b) => EqProp (Two a b) where (=-=) = eq

-- Use checkers to verify the Applicative and Functor for Identity are valid
verifyTwoIsFunctor :: IO ()
verifyTwoIsFunctor = quickBatch $ functor (undefined :: Two (Int,Int,Int) (Int,Int,Int))
verifyTwoIsApplicative :: IO ()
verifyTwoIsApplicative = quickBatch $ applicative (undefined :: Two (String,String,String) (Int,Int,Int))
