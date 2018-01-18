{-# LANGUAGE Strict #-}

module StrictList where

data List a = Nil | Cons a (List a) deriving (Show)


-- take' 0 undefined doesn't work because it tries to evaluate undefined
-- take' 0 $ Cons 1 undefined hits bottom too
take' n _   | n <= 0 = Nil
take' _ Nil          = Nil
take' n (Cons x xs)  = (Cons x (take' (n-1) xs))

-- map' undefined Nil hits bottom
map' _ Nil         = Nil
map' f (Cons x xs) = (Cons (f x) (map' f xs))

repeat' x = xs where xs = (Cons x xs)

-- I couldn't make main return 10 elements because repeat' 1 tries to strictly
-- complete all the lists
main = do
  print $ take' 10 $ map' (+1) (repeat' 1)
