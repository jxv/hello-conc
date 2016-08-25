module HelloTVar.Broadcast.Types
  ( Seconds(..)
  ) where

newtype Seconds = Seconds Int
  deriving (Show, Eq, Num)
