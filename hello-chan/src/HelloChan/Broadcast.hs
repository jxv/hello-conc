module HelloChan.Broadcast
  ( Seconds(..)
  , Broadcaster(..)
  , Delayer(..)
  , HasNumber(..)
  , main
  , BroadcastM
  , runIO
  ) where

import Control.Monad.Reader (ReaderT, runReaderT, MonadReader(..))
import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Exception.Safe (MonadCatch, MonadThrow)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Concurrent (threadDelay)
import Data.Text (Text, unpack, pack)

newtype Seconds = Seconds Int
  deriving (Show, Eq, Num)


class Monad m => Broadcaster m where
  broadcast :: Text -> m ()

class Monad m => Delayer m where
  delay :: Seconds -> m ()

class Monad m => HasNumber m where
  getNumber :: m Int


main :: (Broadcaster m, Delayer m, HasNumber m) => m ()
main = do
  number <- getNumber
  let textNumber = pack (show number)
  broadcast $ "Hello, World! - " `mappend` textNumber
  delay 5
  broadcast $ "Bye, World! - " `mappend`textNumber


newtype BroadcastM a = BroadcastM { unBroadcastM :: ExceptT Text (ReaderT (Text -> IO (), Int) IO) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadError Text, MonadCatch, MonadThrow, MonadReader (Text -> IO (), Int))

runIO :: MonadIO m => BroadcastM a -> (Text -> IO (), Int) -> m a
runIO (BroadcastM m) env@(sender, number) = liftIO $ do
  result <- runReaderT (runExceptT m) env
  either (error . unpack) return result

instance Delayer BroadcastM where
  delay (Seconds secs) = liftIO $ threadDelay (scale * secs)
    where
      scale = 10 ^ 6

instance Broadcaster BroadcastM where
  broadcast msg = do
    (sender, _) <- ask
    liftIO $ sender msg

instance HasNumber BroadcastM where
  getNumber = snd <$> ask
