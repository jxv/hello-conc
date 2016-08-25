module HelloTVar.Broadcast.System
  ( System
  , io
  ) where

import Control.Monad.Reader (ReaderT, runReaderT, MonadReader(..))
import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Exception.Safe (MonadCatch, MonadThrow)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Data.Text (Text, unpack)

import qualified HelloTVar.Broadcast.DelayerImpl as Delayer
import HelloTVar.Broadcast.Parts (Delayer(..), HasNumber(..), Broadcaster(..))

newtype System a = System { unSystem :: ExceptT Text (ReaderT (Text -> IO (), Int) IO) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadError Text, MonadCatch, MonadThrow, MonadReader (Text -> IO (), Int))

io :: System a -> (Text -> IO (), Int) -> IO a
io system env@(sender, number) = do
  result <- runReaderT (runExceptT (unSystem system)) env
  either (error . unpack) return result

instance Delayer System where
  delay = Delayer.delay

instance Broadcaster System where
  broadcast msg = do
    (sender, _) <- ask
    liftIO $ sender msg

instance HasNumber System where
  getNumber = snd <$> ask
