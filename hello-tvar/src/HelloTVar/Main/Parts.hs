module HelloTVar.Main.Parts
  ( Control(..)
  , Setter(..)
  , Printy(..)
  , Interthread(..)
  ) where

import Data.Text (Text)

import HelloTVar.Main.Types (TVar)

class Monad m => Control m where
  control :: (Int -> m (), Int) -> m ()

class Monad m => Setter m where
  setter :: (Int -> m (), Int) -> m ()

class Monad m => Printy m where
  printy :: m Int -> m ()

class Monad m => Interthread m where
  newTVar :: a -> m (TVar m a)
  fork :: m () -> m ()
