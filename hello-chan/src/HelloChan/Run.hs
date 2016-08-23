module HelloChan.Run
  ( run
  ) where

import HelloChan.Interthread (Interthread(..), Chan(..))
import HelloChan.Subsystem (Control(..), Broadcast(..), Printy(..))

run :: (Interthread m, Control m, Broadcast m, Printy m) => m ()
run = do
  chan <- newChan
  fork $ control (\number -> fork $ broadcast (_writeChan chan, number), 0)
  printy (_readChan chan)
