module List where
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a = Nil | Cons a (List a) deriving (Eq, Show)

-- Required for checkers
instance (Eq a) => EqProp (List a) where (=-=) = eq

-- QuickCheck arbitrary
instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = do
    x     <- arbitrary
    xs     <- arbitrary
    aList <- elements [Nil, Cons x xs]
    return $ aList

instance Functor (List) where
  fmap f (Cons x xs)  = Cons (f x) (fmap f xs)
  fmap _ (Nil)          = Nil

instance Applicative (List) where
  pure x = Cons x Nil
  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  (<*>) a b = flatMap (\x -> x <$> b) a

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b' Nil = b'
fold f b' (Cons h t) = f h (fold f b' t)
concat' :: List (List a) -> List a
concat' = fold append Nil
-- write this in terms of concat' and fmap
flatMap :: (a' -> List b') -> List a' -> List b'
flatMap f as = concat' $ f <$> as

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

instance Monad (List) where
  return        = pure
  (>>=) cons ma = flatMap ma cons

checkListEither :: IO ()
checkListEither = do
  putStrLn "== Checking List Monad =="
  let a = (undefined :: List (Int, String, Bool))
  quickBatch $ functor a
  quickBatch $ applicative a
  quickBatch $ monad a
