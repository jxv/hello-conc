module HelloChan.Broadcast.System
  ( System
  , io
  ) where

import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Exception.Safe (MonadCatch, MonadThrow)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Data.Text (Text, unpack)

newtype System a = System { unSystem :: ExceptT Text IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadError Text, MonadCatch, MonadThrow)

io :: System a -> (Text -> IO (), Int) -> IO a
io system (sender, number) = do
  result <- runExceptT (unSystem system)
  either (error . unpack) return result
