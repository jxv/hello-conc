module HelloMVar.Printy.Types
  ( Seconds(..)
  ) where

newtype Seconds = Seconds Int
  deriving (Show, Eq, Num)
