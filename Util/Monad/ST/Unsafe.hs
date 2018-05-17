module Util.Monad.ST.Unsafe where

import Prelude (seq)
import Control.Applicative
import Control.Category (Category (..))
import Control.Monad (Monad (..))
import Control.Monad.ST
import Control.Monad.ST.Unsafe
import Data.Maybe (Maybe (..))

unsafeInterleaveWhileJust :: ST s (Maybe a) -> (a -> ST s ()) -> ST s [a]
unsafeInterleaveWhileJust mma f = go
  where
    go = mma >>= unsafeInterleaveST . \ case
        Nothing -> pure []
        Just a -> (a :) <$> unsafeInterleaveST (a `seq` f a *> go)
