module HelloChan.Control.Forker
  ( Forker(..)
  ) where

class Monad m => Forker m where
  forkBroadcast :: Int -> m ()
