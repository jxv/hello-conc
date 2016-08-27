module HelloMVar.Main.System
  ( System
  , io
  ) where

import qualified Control.Concurrent as IO (forkIO)
import qualified Control.Concurrent.MVar as IO (newMVar, takeMVar, putMVar)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Functor (void)

import qualified HelloMVar.Control.Run as Control (run)
import qualified HelloMVar.Control.System as Control (io)
import qualified HelloMVar.Setter.Run as Setter (run)
import qualified HelloMVar.Setter.System as Setter (io)
import qualified HelloMVar.Printy.Run as Printy (run)
import qualified HelloMVar.Printy.System as Printy (io)
import HelloMVar.Main.Parts (Interthread(..), Control(..), Setter(..), Printy(..))
import HelloMVar.Main.Types (MVar(..))

newtype System a = System { unSystem :: IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

io :: System a -> IO a
io = unSystem

instance Interthread System where
  newMVar a = liftIO $ do
    mvar <- IO.newMVar a
    return $ MVar
      { _putMVar = liftIO . IO.putMVar mvar
      , _takeMVar = liftIO (IO.takeMVar mvar)
      }
  fork = liftIO . void . IO.forkIO . unSystem

instance Control System where
  control (forkSetter, val) = liftIO $ Control.io Control.run (io . forkSetter, val)

instance Setter System where
  setter (set, val) = liftIO $ Setter.io Setter.run (io . set, val)

instance Printy System where
  printy recv = liftIO $ Printy.io Printy.run (io recv)
