j :: Monad m => m (m a) -> m a
j mma = mma >>= id

l1 :: Monad m => (a -> b) -> m a -> m b
l1 f ma = ma >>= (\x -> return $ f x)

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f ma mb = ma >>= (\x -> mb >>= (\y -> return $ f x y))

a :: Monad m => m a -> m (a -> b) -> m b
a ma mab = mab >>= (\fab -> ma >>= (\a -> return $ fab a))

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _      = return []
meh (a:as) f  = (:) <$> (f a >>= (\x -> return $ x)) <*> (meh as f)

flipType :: (Monad m) => [m a] -> m [a]
flipType mas = meh mas (\ma -> ma >>= (\a -> return a))
