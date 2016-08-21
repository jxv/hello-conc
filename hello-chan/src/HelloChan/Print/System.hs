module HelloChan.Print.System
  ( System
  , io
  ) where

import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Exception.Safe (MonadCatch, MonadThrow)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Data.Text (Text, unpack)

import qualified HelloChan.Print.ConsoleImpl as Console
import HelloChan.Print.Console (Console(..))

newtype System a = System { unSystem :: ExceptT Text IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadError Text, MonadCatch, MonadThrow)

io :: System a -> IO Text -> IO a
io system receive = do
  result <- runExceptT (unSystem system)
  either (error . unpack) return result

instance Console System where
  stdout = Console.stdout
