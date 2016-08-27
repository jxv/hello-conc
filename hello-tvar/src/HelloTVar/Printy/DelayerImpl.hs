module HelloTVar.Printy.DelayerImpl
  ( delay
  ) where

import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Concurrent (threadDelay)

import HelloTVar.Printy.Types (Seconds(Seconds))

delay :: MonadIO m => Seconds -> m ()
delay (Seconds secs) = liftIO $ threadDelay (scale * secs)
  where
    scale = 10 ^ 6
