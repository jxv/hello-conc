module HelloTVar.Print
  ( Console(..)
  , Receiver(..)
  , Delayer(..)
  , main
  , PrintM
  , runIO
  ) where

import Control.Monad (join, forever)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.Reader (MonadReader(..), ReaderT, runReaderT)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Exception.Safe (MonadCatch, MonadThrow)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Data.Text (Text, unpack, pack)
import qualified Data.Text.IO as T (putStrLn)
import Control.Concurrent (threadDelay)

newtype Seconds = Seconds Int
  deriving (Show, Eq, Num)

class Monad m => Console m where
  writeLine :: Text -> m ()

class Monad m => Receiver m where
  receiveValue :: m Int

class Monad m => Delayer m where
  delay :: Seconds -> m ()

main :: (Console m, Receiver m, Delayer m) => m ()
main = forever step

step :: (Console m, Receiver m, Delayer m) => m ()
step = do
  val <- receiveValue
  writeLine . pack . show $ val
  delay (Seconds 5)

newtype PrintM a = PrintM { unPrintM :: ReaderT (IO Int) (ExceptT Text IO) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadError Text, MonadCatch, MonadThrow, MonadReader (IO Int))

runIO :: MonadIO m => PrintM a -> IO Int -> m a
runIO (PrintM m) receive = liftIO $ do
  result <- runExceptT (runReaderT m receive)
  either (error . unpack) return result

instance Console PrintM where
  writeLine = liftIO . T.putStrLn

instance Receiver PrintM where
  receiveValue = ask >>= \recv -> liftIO recv

instance Delayer PrintM where
  delay (Seconds secs) = liftIO $ threadDelay (scale * secs)
    where
      scale = 10 ^ 6
