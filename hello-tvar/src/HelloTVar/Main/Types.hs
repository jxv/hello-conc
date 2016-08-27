module HelloTVar.Main.Types
  ( TVar(..)
  ) where

data TVar m a = TVar
  { _readTVar :: m a
  , _writeTVar :: a -> m ()
  }
