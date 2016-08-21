module HelloChan.Console
  ( Console(..)
  ) where

import Data.Text (Text)

class Monad m => Console m where
  sysArg :: m Text
  stdout :: Text -> m ()
