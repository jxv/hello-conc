module HelloChan.Printy.Run
  ( run
  , go
  ) where

import Control.Monad (forever)

import HelloChan.Printy.Console (Console(stdout))
import HelloChan.Printy.Receiver (Receiver(receiveMessage))

run :: (Console m, Receiver m) => m ()
run = forever go

go :: (Console m, Receiver m) => m ()
go = do
  message <- receiveMessage
  stdout message
