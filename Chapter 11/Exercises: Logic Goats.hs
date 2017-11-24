{-# LANGUAGE FlexibleInstances #-}

module LogicGoats where

-- 1. 
class TooMany a where
  tooMany :: a -> Bool

instance TooMany (Int, String) where
  tooMany (n,_) = n > 42

-- 2.
instance TooMany (Int, Int) where
  tooMany (x,y) = (x+y) > 42

-- 3.
instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany (x, y) = tooMany (x + y)
