module HelloChan.Broadcast.Broadcaster
  ( Broadcaster(..)
  ) where

import Data.Text (Text)

class Monad m => Broadcaster m where
  broadcast :: Text -> m ()
