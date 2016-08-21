module HelloChan.Print.System
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

import qualified HelloChan.Print.ConsoleImpl as Console
import HelloChan.Print.Console (Console(..))
import HelloChan.Print.Receiver (Receiver(..))

newtype System a = System { unSystem :: ReaderT (IO Text) (ExceptT Text IO) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadError Text, MonadCatch, MonadThrow, MonadReader (IO Text))

io :: System a -> IO Text -> IO a
io system receive = do
  result <- runExceptT (runReaderT (unSystem system) receive)
  either (error . unpack) return result

instance Console System where
  stdout = Console.stdout

instance Receiver System where
  receiveMessage = ask >>= \recvMsg -> liftIO recvMsg
