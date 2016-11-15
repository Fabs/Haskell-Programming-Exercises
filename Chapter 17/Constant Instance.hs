{-# LANGUAGE InstanceSigs #-}
newtype Constant a b =
  Constant { getConstant :: a }
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Monoid m => Applicative (Constant m) where
  pure a = Constant mempty
  (<*>) :: Constant m (a -> b) -> Constant m a -> Constant m b
  (Constant m1) <*> (Constant m2) = Constant $ m1 `mappend` m2
  --(<*>) (Constant m1) (Constant m2) = Constant _
