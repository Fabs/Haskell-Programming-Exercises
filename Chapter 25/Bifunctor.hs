class Bifunctor p where
  {-# MINIMAL bimap | first, second #-}
  
  bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
  bimap f g = first f . second g

  first :: (a -> b) -> p a c -> p b c
  first f = bimap f id

  second :: (b -> c) -> p a b -> p a c
  second = bimap id

-- 1.
data Deux a b = Deux a b

instance Bifunctor Deux where
  bimap f g (Deux a c) = Deux (f a) (g c)

-- 2.
data Const a b = Const a

instance Bifunctor Const where
  bimap f _ (Const a) = Const (f a)

-- 3.
data Drei a b c = Drei a b c

instance Bifunctor (Drei a) where
  bimap f g (Drei e a c) = Drei e (f a) (g c)
