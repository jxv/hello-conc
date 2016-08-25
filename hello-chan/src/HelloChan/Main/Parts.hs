module HelloChan.Main.Parts
  ( Control(..)
  , Broadcast(..)
  , Printy(..)
  , Interthread(..)
  ) where

import Data.Text (Text)

import HelloChan.Main.Types (Chan)

class Monad m => Control m where
  control :: (Int -> m (), Int) -> m ()

class Monad m => Broadcast m where
  broadcast :: (Text -> m (), Int) -> m ()

class Monad m => Printy m where
  printy :: m Text -> m ()

class Monad m => Interthread m where
  newChan :: m (Chan m a)
  fork :: m () -> m ()
