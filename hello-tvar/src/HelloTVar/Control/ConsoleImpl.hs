module HelloTVar.Control.ConsoleImpl
  ( getLine
  ) where

import Prelude hiding (getLine)

import qualified Data.Text.IO as T (getLine)
import Data.Text (Text)
import Control.Monad.IO.Class (MonadIO(liftIO))

getLine :: MonadIO m => m Text
getLine = liftIO T.getLine
