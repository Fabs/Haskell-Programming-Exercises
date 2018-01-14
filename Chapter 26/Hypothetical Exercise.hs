-- Hypothetical Exercise

{-
Consider ReaderT r Maybe and MaybeT (Reader r) - are these types equivalent?
Do they do the same thing? Try writing otherwise similar bits of code with each
and see if you can prove they're the same or different.

I'm not sure. I tried to construct something with the types but wasn't successful.

-}

import Control.Monad.Trans.Reader
import Control.Monad.Trans.Maybe
--import Control.Monad.Trans.Class
--import Control.Monad.IO.Class

-- ReaderT :: (r -> m a) -> ReaderT r m a
-- ReaderT :: (r -> Maybe a) -> ReaderT r Maybe a
readerMaybe :: ReaderT r Maybe Int
readerMaybe = ReaderT $ \_ -> Just 1

-- MaybeT :: m (Maybe a) -> MaybeT m a
-- MaybeT :: Reader (Maybe a) -> MaybeT Reader a
maybeReader :: MaybeT (Reader r) Int
maybeReader = undefined -- MaybeT $ (ReaderT . const) (\_ -> Just 1) Identity

{- Here is a comparision:
https://github.com/yamad/haskellbook-hpfp/blob/master/Ch26.hs#L178

I was tripped up on the type for MaybeT.
-}

-- a configuration that might fail, works both ways
rm :: ReaderT Int Maybe Int
rm =
  ReaderT $
  \r ->
     if r > 0
       then Just (r + 1)
       else Nothing

mr :: MaybeT ((->) Int) Int
mr =
  MaybeT $
  \r ->
     if r > 0
       then Just (r + 1)
       else Nothing

-- a way to sum over
rm' :: ReaderT [Int] Maybe Int
rm' =
  ReaderT $
  \r ->
     if r == []
       then Nothing
       else Just $ sum r

mr' :: MaybeT ((->) [Int]) Int
mr' =
  MaybeT $
  \r ->
     if null r
       then Nothing
else Just $ sum r


-- Actually using these functions.
test_hypothetical :: IO ()
test_hypothetical = do
  let rm_run =(runReaderT rm) 1
  putStrLn $ "rm result: " ++ (show rm_run)

  let mr_run =(runMaybeT mr) 1
  putStrLn $ "mr result: " ++ (show mr_run)

  if rm_run == mr_run
  then putStrLn "The result is equal."
  else putStrLn "The result are NOT equal."

-- The difference is that they require different monad transformers,
-- otherwise they can work the same.
