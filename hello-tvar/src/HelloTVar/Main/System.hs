module HelloTVar.Main.System
  ( System
  , io
  ) where

import qualified Control.Concurrent as IO (forkIO)
import qualified Control.Concurrent.STM as IO (newTVarIO, readTVarIO, writeTVar, atomically)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Functor (void)

import qualified HelloTVar.Control.Run as Control (run)
import qualified HelloTVar.Control.System as Control (io)
import qualified HelloTVar.Setter.Run as Setter (run)
import qualified HelloTVar.Setter.System as Setter (io)
import qualified HelloTVar.Printy.Run as Printy (run)
import qualified HelloTVar.Printy.System as Printy (io)
import HelloTVar.Main.Parts (Interthread(..), Control(..), Setter(..), Printy(..))
import HelloTVar.Main.Types (TVar(..))

newtype System a = System { unSystem :: IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

io :: System a -> IO a
io = unSystem

instance Interthread System where
  newTVar a = liftIO $ do
    tvar <- IO.newTVarIO a
    return $ TVar
      { _writeTVar = liftIO . IO.atomically . IO.writeTVar tvar
      , _readTVar = liftIO (IO.readTVarIO tvar)
      }
  fork = liftIO . void . IO.forkIO . unSystem

instance Control System where
  control (forkSetter, val) = liftIO $ Control.io Control.run (io . forkSetter, val)

instance Setter System where
  setter (set, val) = liftIO $ Setter.io Setter.run (io . set, val)

instance Printy System where
  printy recv = liftIO $ Printy.io Printy.run (io recv)
