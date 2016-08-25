module HelloChan.Broadcast.Parts
  ( Broadcaster(..)
  , Delayer(..)
  , HasNumber(..)
  ) where

import Data.Text (Text)

import HelloChan.Broadcast.Types (Seconds)

class Monad m => Broadcaster m where
  broadcast :: Text -> m ()

class Monad m => Delayer m where
  delay :: Seconds -> m ()

class Monad m => HasNumber m where
  getNumber :: m Int
