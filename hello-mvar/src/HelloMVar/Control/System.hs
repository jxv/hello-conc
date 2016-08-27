module HelloMVar.Control.System
  ( System
  , io
  ) where

import Control.Monad.State (StateT, MonadState, get, put, evalStateT)
import Control.Monad.Reader (ReaderT, MonadReader, ask, runReaderT)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Exception.Safe (MonadCatch, MonadThrow)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Data.Text (Text, unpack)

import qualified HelloMVar.Control.ConsoleImpl as Console
import HelloMVar.Control.Parts (Console(..), HasNumber(..), Forker(..))

newtype System a = System { unSystem :: ExceptT Text (ReaderT (Int -> IO ()) (StateT Int IO)) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadError Text, MonadCatch, MonadThrow, MonadState Int, MonadReader (Int -> IO ()))

io :: System a -> (Int -> IO (), Int) -> IO a
io system (f, v) = do
  result <- evalStateT (runReaderT (runExceptT (unSystem system)) f) v
  either (error . unpack) return result

instance Console System where
  getLine = Console.getLine

instance HasNumber System where
  putNumber = put
  getNumber = get

instance Forker System where
  forkSetter number = do
    f <- ask
    liftIO $ f number