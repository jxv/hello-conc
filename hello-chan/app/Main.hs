module Main (main) where

import Data.Functor (void)
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (newChan, writeChan, readChan)

import qualified HelloChan.Control.Run as Control (run)
import qualified HelloChan.Control.System as Control (io)
import qualified HelloChan.Broadcast.Run as Broadcast (run)
import qualified HelloChan.Broadcast.System as Broadcast (io)
import qualified HelloChan.Print.Run as Print (run)
import qualified HelloChan.Print.System as Print (io)

main :: IO ()
main = do
  chan <- newChan
  forkIO_ $ Control.io Control.run (\number -> forkIO_ $ Broadcast.io Broadcast.run (writeChan chan, number), 0)
  Print.io Print.run (readChan chan)

forkIO_ :: IO () -> IO ()
forkIO_ = void . forkIO
