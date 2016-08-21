module HelloChan.Control.Console
  ( Console(..)
  ) where

import Prelude hiding (getLine)

import Data.Text (Text)

class Monad m => Console m where
  getLine :: m Text
