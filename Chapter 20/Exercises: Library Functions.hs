-- Exercises: Library Functions

import Data.Foldable
import Data.Monoid
import Data.Semigroup

-- 1.
-- Cheated by looking at source:
-- http://hackage.haskell.org/package/base-4.10.1.0/docs/src/Data.Foldable.html#sum
sum :: (Foldable t, Num a) => t a -> a
sum = getSum . (Data.Foldable.foldMap Sum)

-- 2.
product :: (Foldable t, Num a) => t a -> a
product = getProduct . (Data.Foldable.foldMap Product)

-- 3.
elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem e ta = foldr ((||) . (==) e) False ta

-- Converted to pointfree notation from
--(\x acc -> if (acc || (x == e))
--           then True
--           else False) False ta

-- 4.
-- I'm not sure if this is right, but I had to add a bounded constraint
-- I think using 'forall a .' is another way for this to work but I don't know how.
-- http://hackage.haskell.org/package/base-4.10.1.0/docs/src/Data.Foldable.html#maximum
minimum :: (Foldable t, Ord a, Bounded a) => t a -> Maybe a
minimum ta
  | Main.null ta   = Nothing
  | otherwise = Just $ getMin $ Data.Foldable.foldMap Min ta

-- 5.
maximum :: (Foldable t, Ord a, Bounded a) => t a -> Maybe a
maximum ta
  | Main.null ta   = Nothing
  | otherwise = Just $ getMax $ Data.Foldable.foldMap Max ta

-- 6.
null :: (Foldable t) => t a -> Bool
null ta = (==) 0 (Main.length ta)

-- 7.
length :: (Foldable t) => t a -> Int
length ta = foldr (const ((+) 1)) 0 ta

-- 8.
toList :: (Foldable t) => t a -> [a]
toList = foldr (:) []

-- 9.
fold :: (Foldable t, Monoid m) => t m -> m
fold = Data.Foldable.foldMap (mappend mempty)

-- 10.
foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap f = foldr (mappend . f) mempty
