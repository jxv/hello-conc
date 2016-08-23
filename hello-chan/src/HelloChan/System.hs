module HelloChan.System
  ( System
  , io
  ) where

import qualified Control.Concurrent as IO (forkIO)
import qualified Control.Concurrent.Chan as IO (newChan, writeChan, readChan)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Functor (void)

import qualified HelloChan.Control.Run as Control (run)
import qualified HelloChan.Control.System as Control (io)
import qualified HelloChan.Broadcast.Run as Broadcast (run)
import qualified HelloChan.Broadcast.System as Broadcast (io)
import qualified HelloChan.Printy.Run as Printy (run)
import qualified HelloChan.Printy.System as Printy (io)
import HelloChan.Interthread (Interthread(..), Chan(..))
import HelloChan.Subsystem (Control(..), Broadcast(..), Printy(..))

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

instance Control System where
  control (forkBroadcast, number) = liftIO $
    Control.io Control.run (io . forkBroadcast, number)

instance Broadcast System where
  broadcast (send, number) = liftIO $
    Broadcast.io Broadcast.run (io . send, number)

instance Printy System where
  printy recv = liftIO $
    Printy.io Printy.run (io recv)
