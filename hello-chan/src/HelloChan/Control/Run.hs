module HelloChan.Control.Run
  ( run
  ) where

import Prelude hiding (getLine)

import HelloChan.Control.Console (Console(getLine))

run :: Console m => m ()
run = return ()
