-- Hit counter
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Data.IORef
import qualified Data.Map as M
--import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import System.Environment (getArgs)
import Web.Scotty.Trans

data Config = Config {
  -- that's one, one click!
  -- two..two clicks
  -- Three BEAUTIFUL clicks! ah ah ahhhh
    counts :: IORef (M.Map Text Integer)
  , prefix :: Text
}

type Scotty = ScottyT Text (ReaderT Config IO)
type Handler = ActionT Text (ReaderT Config IO)

bumpBoomp :: Text -> M.Map Text Integer -> (M.Map Text Integer, Integer)
bumpBoomp k m = case (M.lookup k m) of
  Nothing -> (M.insert k 1 m, 1)
  Just a  -> (M.adjust (+1) k m, (a+1))

app :: Scotty ()
app = get "/:key" $ do
  -- Create map key
  unprefixed <- param "key" :: Handler Text
  p <- lift $ asks prefix
  let key' = mappend p unprefixed
  -- get current and incremented count
  currentMap <- lift $ asks counts >>= (liftIO . readIORef)
  let (newMap, newInteger) = bumpBoomp key' currentMap
  -- Render page
  html $ mconcat [ "<h1>Success! Count was: "
                 , TL.pack $ show newInteger
                 , "</h1>"
                 ]
  -- Update count in memory
  lift $ asks counts >>= (liftIO . flip writeIORef newMap)

main :: IO ()
main = do
  -- TODO: Warn user that only one argument is accepted of type String
  [prefixArg] <- getArgs
  counter <- newIORef M.empty
  let config = Config { prefix = TL.pack prefixArg
                      , counts = counter
                      }
      runR m = runReaderT m config
  scottyT 3000 runR app
