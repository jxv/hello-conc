module HelloChan.Subsystem
  ( Control(..)
  , Broadcast(..)
  , Printy(..)
  ) where

import Data.Text (Text)

class Monad m => Control m where
  control :: (Int -> m (), Int) -> m ()

class Monad m => Broadcast m where
  broadcast :: (Text -> m (), Int) -> m ()

class Monad m => Printy m where
  printy :: m Text -> m ()
