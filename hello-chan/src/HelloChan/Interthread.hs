module HelloChan.Interthread
  ( Interthread(..)
  ) where

import Data.Text (Text)

import HelloChan.Chan (Chan(..))

class Monad m => Interthread m where
  newChan :: m (Chan m a)
  fork :: m () -> m ()
