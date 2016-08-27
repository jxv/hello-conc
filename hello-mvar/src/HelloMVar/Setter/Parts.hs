module HelloMVar.Setter.Parts
  ( Set(..)
  , HasNumber(..)
  ) where

import Data.Text (Text)

class Monad m => Set m where
  set :: Int -> m ()

class Monad m => HasNumber m where
  getNumber :: m Int
