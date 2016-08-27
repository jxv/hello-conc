module HelloTVar.Main.Run
  ( run
  ) where

import HelloTVar.Main.Types (TVar(_writeTVar, _readTVar))
import HelloTVar.Main.Parts (Interthread(fork, newTVar), Control(control), Setter(setter), Printy(printy))

run :: (Interthread m, Control m, Setter m, Printy m) => m ()
run = do
  tValue <- newTVar 0
  fork $ control (\adjust -> fork $ setter (_writeTVar tValue, adjust), 1)
  printy (_readTVar tValue)
