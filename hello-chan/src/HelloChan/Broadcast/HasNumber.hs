module HelloChan.Broadcast.HasNumber
  ( HasNumber(..)
  ) where

class Monad m => HasNumber m where
  getNumber :: m Int
