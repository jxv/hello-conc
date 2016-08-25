module HelloChan.Broadcast.DelayerImpl
  ( delay
  ) where

import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Concurrent (threadDelay)

import HelloChan.Broadcast.Types (Seconds(Seconds))

delay :: MonadIO m => Seconds -> m ()
delay (Seconds secs) = liftIO $ threadDelay (scale * secs)
  where
    scale = 10 ^ 6
