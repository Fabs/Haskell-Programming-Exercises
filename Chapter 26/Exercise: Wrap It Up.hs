-- Exercise: Wrap It Up

import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader

-- Cheated by looking at https://github.com/johnchandlerburnham/haskellbook/blob/master/26/OuterInner.hs#L21
-- I'm still not clear as to why the return is needed. I suspect for the () and IO types.
embedded :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded =  MaybeT $ ExceptT $ ReaderT $ return <$> (const (Right (Just 1)))
