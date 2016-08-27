module HelloMVar.Printy.System
  ( System
  , io
  ) where

import Control.Monad (join)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.Reader (MonadReader(..), ReaderT, runReaderT)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Exception.Safe (MonadCatch, MonadThrow)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Data.Text (Text, unpack)

import qualified HelloMVar.Printy.ConsoleImpl as Console
import qualified HelloMVar.Printy.DelayerImpl as Delayer
import HelloMVar.Printy.Parts (Console(..), Receiver(..), Delayer(..))

newtype System a = System { unSystem :: ReaderT (IO Int) (ExceptT Text IO) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadError Text, MonadCatch, MonadThrow, MonadReader (IO Int))

io :: System a -> IO Int -> IO a
io system receive = do
  result <- runExceptT (runReaderT (unSystem system) receive)
  either (error . unpack) return result

instance Console System where
  stdout = Console.stdout

instance Receiver System where
  receiveValue = ask >>= \recv -> liftIO recv

instance Delayer System where
  delay = Delayer.delay
