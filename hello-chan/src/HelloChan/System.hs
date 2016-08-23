module HelloChan.System
  ( System
  , io
  ) where

import qualified Control.Concurrent as IO (forkIO)
import qualified Control.Concurrent.Chan as IO (Chan, newChan, writeChan, readChan)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Functor (void)

import qualified HelloChan.Control.Run as C (run)
import qualified HelloChan.Control.System as C (io)
import qualified HelloChan.Broadcast.Run as B (run)
import qualified HelloChan.Broadcast.System as B (io)
import qualified HelloChan.Print.Run as P (run)
import qualified HelloChan.Print.System as P (io)
import HelloChan.Interthread (Interthread(..))
import HelloChan.Chan (Chan(..))

newtype System a = System { unSystem :: IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

io :: System a -> IO a
io = unSystem

instance Interthread System where
  newChan = liftIO $ do
    chan <- IO.newChan
    return $ Chan
      { _writeChan = \a -> liftIO $ IO.writeChan chan a
      , _readChan = liftIO (IO.readChan chan)
      }
  fork = liftIO . void . IO.forkIO . unSystem
  runControl (forkBroadcast, number) = liftIO $ C.io C.run (io . forkBroadcast, number)
  runBroadcast (send, number) = liftIO $ B.io B.run (io . send, number)
  runPrint recv = liftIO $ P.io P.run (io recv)
