module HelloChan.Interthread
  ( Interthread(..)
  , Chan(..)
  ) where

import Data.Text (Text)

data Chan m a = Chan
  { _writeChan :: a -> m ()
  , _readChan :: m a
  }

class Monad m => Interthread m where
  newChan :: m (Chan m a)
  fork :: m () -> m ()
