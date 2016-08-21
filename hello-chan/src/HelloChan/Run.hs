module HelloChan.Run
  ( run
  ) where

import HelloChan.Configuration (Configuration(target))

run :: Configuration m => m ()
run = return ()
