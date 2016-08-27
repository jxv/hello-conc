module HelloMVar.Setter.System
  ( System
  , io
  ) where

import Control.Monad.Reader (ReaderT, runReaderT, MonadReader(..))
import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Exception.Safe (MonadCatch, MonadThrow)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Data.Text (Text, unpack)
import Data.Functor (void)

import HelloMVar.Setter.Parts (HasNumber(..), Set(..))

newtype System a = System { unSystem :: ExceptT Text (ReaderT (Int -> IO (), Int) IO) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadError Text, MonadCatch, MonadThrow, MonadReader (Int -> IO (), Int))

io :: System a -> (Int -> IO (), Int) -> IO a
io system env = do
  result <- runReaderT (runExceptT (unSystem system)) env
  either (error . unpack) return result

instance Set System where
  set val = do
    f <- fst <$> ask
    liftIO $ f val

instance HasNumber System where
  getNumber = snd <$> ask
