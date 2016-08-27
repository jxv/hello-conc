module HelloMVar.Main.Types
  ( MVar(..)
  ) where

data MVar m a = MVar
  { _takeMVar :: m a
  , _putMVar :: a -> m ()
  }
