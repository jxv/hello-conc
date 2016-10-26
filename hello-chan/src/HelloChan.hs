module HelloChan
  ( Wires(..)
  , Interthread(..)
  , main
  , HelloChan
  , runIO
  ) where

import qualified Control.Concurrent as IO (forkIO)
import qualified Control.Concurrent.Chan as IO (newChan, writeChan, readChan)
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Functor (void)
import Data.Text (Text)

import qualified HelloChan.Control as Control (main, runIO)
import qualified HelloChan.Broadcast as Broadcast (main, runIO)
import qualified HelloChan.Print as Print (main, runIO)

data Chan m a = Chan
  { _writeChan :: a -> m ()
  , _readChan :: m a
  }

class Monad m => Wires m where
  runControl :: (Int -> m (), Int) -> m ()
  runBroadcast :: (Text -> m (), Int) -> m ()
  runPrint :: m Text -> m ()

class Monad m => Interthread m where
  newChan :: m (Chan m a)
  fork :: m () -> m ()

main :: (Interthread m, Wires m) => m ()
main = do
  chan <- newChan
  fork $ runControl (\number -> fork $ runBroadcast (_writeChan chan, number), 0)
  runPrint (_readChan chan)

newtype HelloChan a = HelloChan { unHelloChan :: IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

runIO :: HelloChan a -> IO a
runIO = unHelloChan

instance Interthread HelloChan where
  newChan = liftIO $ do
    chan <- IO.newChan
    return $ Chan
      { _writeChan = \a -> liftIO $ IO.writeChan chan a
      , _readChan = liftIO (IO.readChan chan)
      }
  fork = liftIO . void . IO.forkIO . unHelloChan

instance Wires HelloChan where
  runControl (forkBroadcast, number) = Control.runIO Control.main (runIO . forkBroadcast, number)
  runBroadcast (send, number) = Broadcast.runIO Broadcast.main (runIO . send, number)
  runPrint recv = Print.runIO Print.main (runIO recv)
