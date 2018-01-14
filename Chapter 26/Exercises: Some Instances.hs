-- Exercises: Some Instances
-- Copied from: https://github.com/johnchandlerburnham/haskellbook/blob/master/26/MaybeT.hs

-- StateT
import Control.Monad.Trans

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance (Functor m) => Functor (MaybeT m) where
  fmap f (MaybeT ma) = MaybeT $ (fmap . fmap) f ma

instance (Applicative m) => Applicative (MaybeT m) where
  pure x = MaybeT (pure (pure x))
  MaybeT fab <*> MaybeT fa = MaybeT $ fmap (<*>) fab <*> fa

instance (Monad m) => Monad (MaybeT m) where
  return = pure
  MaybeT ma >>= f = MaybeT $ do
    v <- ma
    case v of
      Nothing -> return Nothing
      Just a  -> runMaybeT $ f a

-- 1. MaybeT
instance MonadTrans MaybeT where
  lift ma = MaybeT $ fmap Just ma

instance (MonadIO m) => MonadIO (MaybeT m) where
  liftIO = lift . liftIO
