-- Chapter Exercises

-- 1.
data Constant a b = Constant b

instance Foldable (Constant a) where
  foldr f i (Constant a) = f a i

-- 2.
data Two a b = Two a b

instance Foldable (Two a) where
  foldr f i (Two _ b) = f b i

-- 3.
data Three a b c = Three a b c

instance Foldable (Three a b) where
  foldr f i (Three _ _ c) = f c i

-- 4.
data Three' a b = Three' a b b

instance Foldable (Three' a) where
  foldr f i (Three' _ b1 b2) = f b2 (f b1 i)

-- 5.
data Four' a b = Four' a b b b

instance Foldable (Four' a) where
  foldr f i (Four' _ b1 b2 b3) = f b3 $ f b2 $ f b1 i


-- Thinking cap time.
-- But I cheated: https://github.com/dmvianna/haskellbook/blob/master/src/Ch20-Foldable.hs#L97
-- 
-- (g a) will be a Bool which determines if the element has been found. Then we just need
-- to lift a using pure or return mempty for the monoid return which is of type (Monoid (f a))
filterF :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF g ta = foldMap (\a -> if g a then pure a else mempty) ta
