module HelloMVar.Main.Run
  ( run
  ) where

import HelloMVar.Main.Types (MVar(_putMVar, _takeMVar))
import HelloMVar.Main.Parts (Interthread(fork, newMVar), Control(control), Setter(setter), Printy(printy))

run :: (Interthread m, Control m, Setter m, Printy m) => m ()
run = do
  mValue <- newMVar 0
  fork $ control (\adjust -> fork $ setter (_putMVar mValue, adjust), 1)
  printy (_takeMVar mValue)
