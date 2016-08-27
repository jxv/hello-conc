module HelloMVar.Printy.ConsoleImpl
  ( stdout
  ) where

import qualified Data.Text.IO as T (putStrLn)
import Data.Text (Text)
import Control.Monad.IO.Class (MonadIO(liftIO))

stdout :: MonadIO m => Text -> m ()
stdout = liftIO . T.putStrLn
