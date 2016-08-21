module HelloChan.Control.System
  ( System
  , io
  ) where

import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Exception.Safe (MonadCatch, MonadThrow)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Data.Text (Text, unpack)

import qualified HelloChan.Control.ConsoleImpl as Console
import HelloChan.Control.Console (Console(..))

newtype System a = System { unSystem :: ExceptT Text IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadError Text, MonadCatch, MonadThrow)

io :: System a -> IO a
io system = do
  result <- runExceptT (unSystem system)
  either (error . unpack) return result

instance Console System where
  getLine = Console.getLine
