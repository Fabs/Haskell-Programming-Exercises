module Identity where
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

newtype Identity a = Identity a deriving (Eq, Ord, Show)

-- Required for checkers
instance (Eq a) => EqProp (Identity a) where (=-=) = eq

-- QuickCheck arbitrary
instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = do
    x <- arbitrary
    return $ Identity x

instance Functor (Identity) where
  fmap f (Identity a) = Identity (f a)

instance Applicative (Identity) where
  pure = Identity
  (<*>) (Identity x) (Identity y) = Identity (x y)

instance Monad (Identity) where
  return  = pure
  (>>=) (Identity x) ma = ma x

checkIdentityEither :: IO ()
checkIdentityEither = do
  putStrLn "== Checking Identity Monad =="
  let a = (undefined :: Identity (Int, String, Bool))
  quickBatch $ functor a
  quickBatch $ applicative a
  quickBatch $ monad a
