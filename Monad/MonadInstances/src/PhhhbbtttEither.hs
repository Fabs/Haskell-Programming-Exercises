module PhhhbbtttEither where
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data PhhhbbtttEither b a = Left a | Right b deriving (Eq, Show)

-- Required for checkers
instance (Eq a, Eq b) => EqProp (PhhhbbtttEither b a) where (=-=) = eq

-- QuickCheck arbitrary
instance (Arbitrary a, Arbitrary b) => Arbitrary (PhhhbbtttEither b a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    aPhhhbbtttEither <- elements [PhhhbbtttEither.Left x, PhhhbbtttEither.Right y]
    return $ aPhhhbbtttEither

instance Functor (PhhhbbtttEither b) where
  fmap _ (PhhhbbtttEither.Right b)  = (PhhhbbtttEither.Right b)
  fmap f (PhhhbbtttEither.Left b) = (PhhhbbtttEither.Left (f b))

checkPhhhbbtttEither :: IO ()
checkPhhhbbtttEither = do
  putStrLn "== Checking PhhhbbtttEither Monad =="
  let a = (undefined :: PhhhbbtttEither (Int, String, Int) (String,Int,Int))
  quickBatch $ functor a
--  quickBatch $ applicative a
--  quickBatch $ monad a
