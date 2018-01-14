import Prelude hiding (Left, Right, Either)

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

-- 4.
data SuperDrei a b c = SuperDrei a b

instance Bifunctor (SuperDrei a) where
  bimap f _ (SuperDrei e a) = SuperDrei e (f a)

-- 5.
data SemiDrei a b c = SemiDrei a

instance Bifunctor (SemiDrei a) where
  bimap _ _ (SemiDrei e) = SemiDrei e

-- 6.
data Quadriceps a b c d = Quadzzz a b c d

instance Bifunctor (Quadriceps a b) where
  bimap f g (Quadzzz e1 e2 a c) = Quadzzz e1 e2 (f a) (g c)

-- 7.
data Either a b = Left a | Right b

instance Bifunctor Either where
  bimap f _ (Left a) = Left (f a)
  bimap _ g (Right c) = Right (g c)
