module HelloChan.System
  ( System
  , io
  ) where

import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Exception.Safe (MonadCatch, MonadThrow)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Data.Text (Text, unpack)

import qualified HelloChan.ConsoleImpl as Console
import qualified HelloChan.ConfigurationImpl as Configuration
import HelloChan.Console (Console(..))
import HelloChan.Configuration (Configuration(..))

newtype System a = System { unSystem :: ExceptT Text IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadError Text, MonadCatch, MonadThrow)

io :: System a -> IO a
io system = do
  result <- runExceptT (unSystem system)
  either (error . unpack) return result

instance Console System where
  sysArg = Console.sysArg
  stdout = Console.stdout

instance Configuration System where
  target = Configuration.target
