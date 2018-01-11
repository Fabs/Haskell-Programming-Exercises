-- instiance types provided as they may help.

{-# LANGUAGE InstanceSigs #-}

newtype Compose f g a = Compose { getCompose :: f (g a) } deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

-- Help from here: https://github.com/MattMSumner/haskell-progamming/blob/master/chapter25/twinplicative.hs#L12
--
-- pure requires the f ang g applicative in order to comple the Compose
-- <*> seems to require two <*> because it needs to handle the f and g applicative
--     the fmap is used to apply the ap to the g applicative
instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure a = Compose (pure (pure a))

  (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
  (Compose fgab) <*> (Compose fga) = Compose ((fmap (<*>) fgab) <*> fga)

-- Exercises: Compose Instances

-- 1. Write the Compose Foldable instance
--
-- Must (foldMap f) function over (g a) requiring two foldMap calls
instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldMap f (Compose fga) = (foldMap . foldMap) f fga

-- 2. Write the Compose Traversable instance
-- applied traverse to both f ang g
instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  traverse aTOfb (Compose a) = Compose <$> ((traverse . traverse) aTOfb a)
