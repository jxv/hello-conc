module HelloChan.Printy.Parts
  ( Console(..)
  , Receiver(..)
  ) where

import Data.Text (Text)

class Monad m => Console m where
  stdout :: Text -> m ()

class Monad m => Receiver m where
  receiveMessage :: m Text
