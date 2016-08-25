module HelloChan.Main.Types
  ( Chan(..)
  ) where

data Chan m a = Chan
  { _writeChan :: a -> m ()
  , _readChan :: m a
  }
