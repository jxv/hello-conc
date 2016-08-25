module HelloTVar.Control.Parts
  ( Console(..)
  , Forker(..)
  , HasNumber(..)
  ) where

import Prelude hiding (getLine)

import Data.Text (Text)

class Monad m => Console m where
  getLine :: m Text

class Monad m => Forker m where
  forkBroadcast :: Int -> m ()

class Monad m => HasNumber m where
  getNumber :: m Int
  putNumber :: Int -> m ()
