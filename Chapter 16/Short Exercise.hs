-- Short Exercise

-- 1.
data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a)  = First a
  fmap f (Second a) = Second (f a)

{- 2. Why is a Functor instance that applies the function on to First, Either's Left, impossible?

Because the Functor instance has to be of kind * -> * which means the right most
types need to be the types which are applied to the Functor function.
-}
