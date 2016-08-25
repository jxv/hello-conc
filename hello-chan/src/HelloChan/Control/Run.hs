module HelloChan.Control.Run
  ( run
  , step
  ) where

import Prelude hiding (getLine)

import Control.Monad (forever)

import HelloChan.Control.Parts
  ( Console(getLine)
  , HasNumber(getNumber, putNumber)
  , Forker(forkBroadcast)
  )

run :: (Console m, HasNumber m, Forker m) => m ()
run = forever step

step :: (Console m, HasNumber m, Forker m) => m ()
step = do
  _ <- getLine
  number <- getNumber
  forkBroadcast number
  putNumber (number + 1)
