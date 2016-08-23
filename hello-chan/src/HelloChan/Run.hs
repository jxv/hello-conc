module HelloChan.Run
  ( run
  ) where

import HelloChan.Interthread (Interthread(..))
import HelloChan.Subsystem (Control(..), Broadcast(..), Printy(..))
import HelloChan.Chan (Chan(..))

run :: (Interthread m, Control m, Broadcast m, Printy m) => m ()
run = do
  chan <- newChan
  fork $ control (\number -> fork $ broadcast (_writeChan chan, number), 0)
  printy (_readChan chan)
