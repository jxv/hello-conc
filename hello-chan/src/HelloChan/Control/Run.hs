module HelloChan.Control.Run
  ( run
  , go
  ) where

import Prelude hiding (getLine)

import Control.Monad (forever)

import HelloChan.Control.Console (Console(getLine))
import HelloChan.Control.HasNumber (HasNumber(getNumber, putNumber))
import HelloChan.Control.Forker (Forker(forkBroadcast))

run :: (Console m, HasNumber m, Forker m) => m ()
run = forever go

go :: (Console m, HasNumber m, Forker m) => m ()
go = do
  _ <- getLine
  number <- getNumber
  forkBroadcast number
  putNumber (number + 1)
