module HelloChan.Print.Run
  ( run
  , go
  ) where

import Control.Monad (forever)

import HelloChan.Print.Console (Console(stdout))
import HelloChan.Print.Receiver (Receiver(receiveMessage))

run :: (Console m, Receiver m) => m ()
run = forever go

go :: (Console m, Receiver m) => m ()
go = do
  message <- receiveMessage
  stdout message
