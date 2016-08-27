module HelloMVar.Printy.Parts
  ( Console(..)
  , Receiver(..)
  , Delayer(..)
  ) where

import Data.Text (Text)

import HelloMVar.Printy.Types (Seconds)

class Monad m => Console m where
  stdout :: Text -> m ()

class Monad m => Receiver m where
  receiveValue :: m Int

class Monad m => Delayer m where
  delay :: Seconds -> m ()
