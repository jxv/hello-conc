module HelloChan.Run
  ( run
  ) where

import HelloChan.Interthread (Interthread(..))
import HelloChan.Chan (Chan(..))

run :: Interthread m => m ()
run = do
  chan <- newChan
  fork $ runControl
    (\number -> fork $ runBroadcast (_writeChan chan, number), 0)
  runPrint (_readChan chan)
