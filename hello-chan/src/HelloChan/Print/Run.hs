module HelloChan.Print.Run
  ( run
  ) where

import HelloChan.Print.Console (Console(stdout))

run :: Console m => m ()
run = return ()
