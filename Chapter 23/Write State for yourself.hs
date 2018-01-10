{-# LANGUAGE InstanceSigs #-}

module State where

newtype Moi s a = Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) = Moi (\s -> ( (f $ fst (g s)), s))

instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi (\s -> (a, s))
  (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
  Moi f <*> Moi g = Moi $ \s -> ((fst (f s)) (fst (g s)), s)

-- Got a little bit of help because my first implementation failed during chapter exercises
-- I was using the first s type twice which was throwing away new states.
--
-- https://github.com/johnchandlerburnham/haskellbook/blob/master/23/ChapterExercises.hs#L13
instance Monad (Moi s) where
  return = pure
  (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
  (Moi f) >>= g = Moi $ \s -> let (a, s') = f s
                              in (runMoi $ g a) s'
