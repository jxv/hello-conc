module HelloMVar.Main.Parts
  ( Control(..)
  , Setter(..)
  , Printy(..)
  , Interthread(..)
  ) where

import Data.Text (Text)

import HelloMVar.Main.Types (MVar)

class Monad m => Control m where
  control :: (Int -> m (), Int) -> m ()

class Monad m => Setter m where
  setter :: (Int -> m (), Int) -> m ()

class Monad m => Printy m where
  printy :: m Int -> m ()

class Monad m => Interthread m where
  newMVar :: a -> m (MVar m a)
  fork :: m () -> m ()
