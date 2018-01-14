import Control.Monad.Trans

-- Exercise: StateT
newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }

-- 1. Functor
instance (Functor m) => Functor (StateT s m) where
  fmap f (StateT smas) = StateT (\s ->
    let mas = smas s
        mbs = fmap (\(a, s') -> (f a, s')) mas
    in mbs)

-- 2. Applicative
-- Got some help from these sources because my `s` were applied out of order it seemed:
-- https://github.com/johnchandlerburnham/haskellbook/blob/master/26/StateT.hs#L11
-- https://github.com/data61/fp-course/issues/134
instance (Monad m) => Applicative (StateT s m) where
  pure a = StateT (\s -> pure (a, s))
  (StateT smf) <*> (StateT sma) = StateT $ \s ->
    smf s  >>= \(f, s1) ->
    sma s1 >>= \(a, s2) ->
    return (f a, s2)

-- 3. Monad
instance (Monad m) => Monad (StateT s m) where
  return = pure
  (StateT sma) >>= f = StateT $ \s -> do
    (a, s1) <- sma s
    runStateT (f a) s1

-- Exercises: Lift More
-- Got help here: https://github.com/johnchandlerburnham/haskellbook/blob/master/26/StateT.hs#L24
-- I had forgottne that I needed to use (>>=) so that I could apply `a` with the new `s`
instance MonadTrans (StateT s) where
  lift ma = StateT (\s -> ma >>= (\a -> return (a, s)))

instance (MonadIO m) => MonadIO (StateT s m) where
  liftIO = lift . liftIO
