module HelloMVar
  ( Seconds(..)
  , Wires(..)
  , Interthread(..)
  , main
  , HelloMVarM
  , runIO
  ) where

import qualified Control.Concurrent as IO (forkIO)
import qualified Control.Concurrent.MVar as IO (newMVar, takeMVar, putMVar)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Functor (void)

import qualified HelloMVar.Control as Control (runIO, main)
import qualified HelloMVar.Assignment as Assignment (runIO, main)
import qualified HelloMVar.Print as Print (runIO, main)

newtype Seconds = Seconds Int
  deriving (Show, Eq, Num)

data MVar m a = MVar
  { _takeMVar :: m a
  , _putMVar :: a -> m ()
  }

class Monad m => Wires m where
  runControl :: (Int -> m (), Int) -> m ()
  runAssignment :: (Int -> m (), Int) -> m ()
  runPrint :: m Int -> m ()

class Monad m => Interthread m where
  newMVar :: a -> m (MVar m a)
  fork :: m () -> m ()


main :: (Interthread m, Wires m) => m ()
main = do
  mvar <- newMVar 0
  fork $ runControl (\adjust -> fork $ runAssignment (_putMVar mvar, adjust), 1)
  runPrint (_takeMVar mvar)


newtype HelloMVarM a = HelloMVarM { unHelloMVarM :: IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

runIO :: HelloMVarM a -> IO a
runIO = unHelloMVarM

instance Interthread HelloMVarM where
  newMVar a = liftIO $ do
    mvar <- IO.newMVar a
    return $ MVar
      { _putMVar = liftIO . IO.putMVar mvar
      , _takeMVar = liftIO (IO.takeMVar mvar)
      }
  fork = liftIO . void . IO.forkIO . unHelloMVarM

instance Wires HelloMVarM where
  runControl (forkSetter, val) = Control.runIO Control.main (runIO . forkSetter, val)
  runAssignment (set, val) = Assignment.runIO Assignment.main (runIO . set, val)
  runPrint recv = Print.runIO Print.main (runIO recv)
