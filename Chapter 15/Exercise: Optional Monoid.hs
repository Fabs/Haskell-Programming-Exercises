import Data.Monoid

import Test.QuickCheck

-- Exercise: Optional Monoid

data Optional a = Nada | Only a deriving (Eq, Show)

instance (Arbitrary a) => Arbitrary (Optional a) where
  arbitrary = do
    a <- arbitrary
    oneof [ return Nada
          , return $ Only a
          ]

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend (Only a) (Only b) = Only (a <> b)
  mappend Nada a            = a
  mappend a Nada            = a

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

-- Exercise: Maybe Another Monoid

newtype First' a = First' { getFirst' :: Optional a }
  deriving (Eq, Show)

instance (Arbitrary a) => Arbitrary (First' a) where
  arbitrary = do
    a <- arbitrary
    return $ First' a

instance Monoid (First' a) where
  mempty      = First' Nada
  mappend (First' Nada) a = a
  mappend a (First' Nada) = a
  mappend a _ = a

firstMappend :: First' a
             -> First' a
             -> First' a
firstMappend = mappend

type FirstMappend =
  First' String -> First' String -> First' String -> Bool

type FstId = First' String -> Bool

main :: IO ()
main = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FstId)
  quickCheck (monoidRightIdentity :: FstId)
