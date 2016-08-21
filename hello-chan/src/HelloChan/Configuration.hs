module HelloChan.Configuration
  ( Configuration(..)
  ) where

import Data.Text (Text)

class Monad m => Configuration m where
  target :: m Text
