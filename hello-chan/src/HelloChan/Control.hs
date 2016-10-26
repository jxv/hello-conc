module HelloChan.Control
  ( Console(..)
  , Forker(..)
  , HasNumber(..)
  , main
  , step
  , System
  , runIO
  ) where

import Control.Monad (forever)
import Control.Monad.State (StateT, MonadState, get, put, evalStateT)
import Control.Monad.Reader (ReaderT, MonadReader, ask, runReaderT)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Exception.Safe (MonadCatch, MonadThrow)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Data.Text (Text, unpack)
import qualified Data.Text.IO as T (getLine)

class Monad m => Console m where
  readLine :: m Text

class Monad m => Forker m where
  forkBroadcast :: Int -> m ()

class Monad m => HasNumber m where
  getNumber :: m Int
  putNumber :: Int -> m ()


main :: (Console m, HasNumber m, Forker m) => m ()
main = forever step

step :: (Console m, HasNumber m, Forker m) => m ()
step = do
  _ <- readLine
  number <- getNumber
  forkBroadcast number
  putNumber (number + 1)


newtype System a = System { unSystem :: ExceptT Text (ReaderT (Int -> IO ()) (StateT Int IO)) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadError Text, MonadCatch, MonadThrow, MonadState Int, MonadReader (Int -> IO ()))

runIO :: MonadIO m => System a -> (Int -> IO (), Int) -> m a
runIO system (broadcast, number) = liftIO $ do
  result <- evalStateT (runReaderT (runExceptT (unSystem system)) broadcast) number
  either (error . unpack) return result

instance Console System where
  readLine = liftIO T.getLine

instance HasNumber System where
  putNumber = put
  getNumber = get

instance Forker System where
  forkBroadcast number = do
    f <- ask
    liftIO $ f number
