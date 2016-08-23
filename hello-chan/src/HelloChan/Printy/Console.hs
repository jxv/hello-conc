module HelloChan.Printy.Console
  ( Console(..)
  ) where

import Data.Text (Text)

class Monad m => Console m where
  stdout :: Text -> m ()
