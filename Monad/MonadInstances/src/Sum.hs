module Sum where
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Sum a b = First a | Second b deriving (Eq, Show)

-- Required for checkers
instance (Eq a, Eq b) => EqProp (Sum a b) where (=-=) = eq

-- QuickCheck arbitrary
instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    aSum <- elements [First x, Second y]
    return $ aSum

instance Functor (Sum a) where
  fmap _ (First a)  = (First a)
  fmap f (Second b) = (Second (f b))

verifySumIsFunctor :: IO ()
verifySumIsFunctor = quickBatch $ functor (undefined :: Sum (Int,Int,Int) (Int,Int,Int))

instance Applicative (Sum a) where
  pure  = Second
  (<*>) (First x) _           = First x
  (<*>) _ (First x)           = First x
  (<*>) (Second a) (Second b) = Second (a b)

verifySumIsApplicative :: IO ()
verifySumIsApplicative = quickBatch $ applicative (undefined :: Sum (Int,Int,Int) (Int,Int,Int))

instance Monad (Sum a) where
  return    = pure
  (>>=) (Second a) ma = ma a
  (>>=) (First a) _   = First a

verifySumIsMonad :: IO ()
verifySumIsMonad = quickBatch $ monad (undefined :: Sum (Int,Int,Int) (Int,Int,Int))
