module HelloTVar.Assignment
  ( Set(..)
  , HasNumber(..)
  , main
  , AssignmentM
  , runIO
  ) where

import Control.Monad.Reader (ReaderT, runReaderT, MonadReader(..))
import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Exception.Safe (MonadCatch, MonadThrow)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Data.Text (Text, unpack)
import Data.Functor (void)

class Monad m => Set m where
  set :: Int -> m ()

class Monad m => HasNumber m where
  getNumber :: m Int


main :: (Set m, HasNumber m) => m ()
main = do
  number <- getNumber
  set number


newtype AssignmentM a = AssignmentM { unAssignmentM :: ExceptT Text (ReaderT (Int -> IO (), Int) IO) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadError Text, MonadCatch, MonadThrow, MonadReader (Int -> IO (), Int))

runIO :: MonadIO m => AssignmentM a -> (Int -> IO (), Int) -> m a
runIO (AssignmentM m) env = liftIO $ do
  result <- runReaderT (runExceptT m) env
  either (error . unpack) return result

instance Set AssignmentM where
  set val = do
    f <- fst <$> ask
    liftIO $ f val

instance HasNumber AssignmentM where
  getNumber = snd <$> ask
