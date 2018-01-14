import Control.Monad.Trans

-- Exercises: EitherT
newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

-- 1. Write the Functor instance for EitherT
instance Functor m => Functor (EitherT e m) where
  fmap f (EitherT m) = EitherT $ (fmap . fmap) f m

-- 2. Write teh Applicative instance for EitherT
instance Applicative m => Applicative (EitherT e m) where
  pure a = EitherT $ pure $ pure a
  EitherT fab <*> EitherT fa = EitherT $ fmap (<*>) fab <*> fa

-- 3. Write the Monad instance for EitherT
instance Monad m => Monad (EitherT e m) where
  return = pure
  EitherT ma >>= f = EitherT $ do
    e <- ma
    case e of
      Left l  -> return $ Left l
      Right a -> runEitherT $ f a

-- 4. Write teh swapEitherT helper function for EitherT
swapEither :: Either e a -> Either a e
swapEither (Right a) = Left a
swapEither (Left e) = Right e

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT me) = EitherT $ swapEither <$> me

-- 5. Write the transformer variant of the either catamorphism
eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT f g (EitherT me) = do
  e <- me
  either f g e

-- Exercises: Lift More
instance MonadTrans (EitherT e) where
  lift = EitherT . fmap Right

instance (MonadIO m) => MonadIO (EitherT e m) where
  liftIO = lift . liftIO
