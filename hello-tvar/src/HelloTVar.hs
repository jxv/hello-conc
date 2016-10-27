module HelloTVar
  ( Wires(..)
  , Interthread(..)
  , main
  , HelloTVarM
  , runIO
  ) where

import Data.Text (Text)

import qualified Control.Concurrent as IO (forkIO)
import qualified Control.Concurrent.STM as IO (newTVarIO, readTVarIO, writeTVar, atomically)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Functor (void)

import qualified HelloTVar.Control as Control (runIO, main)
import qualified HelloTVar.Assignment as Assignment (runIO, main)
import qualified HelloTVar.Print as Print (runIO, main)

data TVar m a = TVar
  { _readTVar :: m a
  , _writeTVar :: a -> m ()
  }

class Monad m => Wires m where
  runControl :: (Int -> m (), Int) -> m ()
  runAssignment :: (Int -> m (), Int) -> m ()
  runPrint :: m Int -> m ()

class Monad m => Interthread m where
  newTVar :: a -> m (TVar m a)
  fork :: m () -> m ()


main :: (Interthread m, Wires m) => m ()
main = do
  tv <- newTVar 0
  fork $ runControl (\adjust -> fork $ runAssignment (_writeTVar tv, adjust), 1)
  runPrint (_readTVar tv)


newtype HelloTVarM a = HelloTVarM { unHelloTVarM :: IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

runIO :: HelloTVarM a -> IO a
runIO = unHelloTVarM

instance Interthread HelloTVarM where
  newTVar a = liftIO $ do
    tvar <- IO.newTVarIO a
    return $ TVar
      { _writeTVar = liftIO . IO.atomically . IO.writeTVar tvar
      , _readTVar = liftIO (IO.readTVarIO tvar)
      }
  fork = liftIO . void . IO.forkIO . unHelloTVarM

instance Wires HelloTVarM where
  runControl (forkSetter, val) = Control.runIO Control.main (runIO . forkSetter, val)
  runAssignment (set, val) = Assignment.runIO Assignment.main (runIO . set, val)
  runPrint recv = Print.runIO Print.main (runIO recv)
