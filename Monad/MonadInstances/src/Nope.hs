module Nope where
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Nope a = NopeDotJpg deriving (Eq, Show)

-- Required for checkers
instance (Eq a) => EqProp (Nope a) where (=-=) = eq

-- QuickCheck arbitrary
instance (Arbitrary a) => Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance Functor (Nope) where
  fmap _ _  = NopeDotJpg

instance Applicative (Nope) where
  pure _    = NopeDotJpg
  (<*>) _ _ = NopeDotJpg

instance Monad (Nope) where
  return    = pure
  (>>=) _ _ = NopeDotJpg

checkNope :: IO ()
checkNope = do
  putStrLn "== Checking Nope Monad =="
  let a = (undefined :: Nope (Int, Int, Int))
  quickBatch $ functor a
  quickBatch $ applicative a
  quickBatch $ monad a
