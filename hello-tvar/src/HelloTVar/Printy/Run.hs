module HelloTVar.Printy.Run
  ( run
  , step
  ) where

import Control.Monad (forever)

import HelloTVar.Printy.Parts (Console(stdout), Receiver(receiveMessage))

run :: (Console m, Receiver m) => m ()
run = forever step

step :: (Console m, Receiver m) => m ()
step = do
  message <- receiveMessage
  stdout message
