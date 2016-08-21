module Main (main) where

import qualified HelloChan.Control.Run as Control (run)
import qualified HelloChan.Control.System as Control (io)
import qualified HelloChan.Broadcast.Run as Broadcast (run)
import qualified HelloChan.Broadcast.System as Broadcast (io)
import qualified HelloChan.Print.Run as Print (run)
import qualified HelloChan.Print.System as Print (io)

main :: IO ()
main = do
  Control.io Control.run
  Broadcast.io Broadcast.run
  Print.io Print.run
