module HelloChan.Broadcast.Delayer
  ( Delayer(..)
  , Seconds(..)
  ) where

newtype Seconds = Seconds Int
  deriving (Show, Eq, Num)

class Monad m => Delayer m where
  delay :: Seconds -> m ()
