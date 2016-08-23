module HelloChan.Interthread
  ( Interthread(..)
  ) where

import Data.Text (Text)

import HelloChan.Chan (Chan(..))

class Monad m => Interthread m where
  newChan :: m (Chan m a)
  fork :: m () -> m ()
  runControl :: (Int -> m (), Int) -> m ()
  runBroadcast :: (Text -> m (), Int) -> m ()
  runPrint :: m Text -> m ()
