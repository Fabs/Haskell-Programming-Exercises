-- Exercises: Instances of Func

import Test.QuickCheck
import Test.QuickCheck.Function

-- Functor laws
functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose' :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g) =
  (fmap (g . f) x) == (fmap g . fmap f $ x)

type IntToInt = Fun Int Int

-- 1.
newtype Identity a = Identity a deriving (Show, Eq)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

type IdentityFC =  Identity Int -> IntToInt -> IntToInt -> Bool

-- 2.

main :: IO ()
main = do
  quickCheck (functorIdentity :: (Identity Int -> Bool))
  quickCheck (functorCompose' :: IdentityFC)
