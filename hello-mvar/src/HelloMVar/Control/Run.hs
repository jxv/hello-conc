module HelloMVar.Control.Run
  ( run
  , step
  ) where

import Prelude hiding (getLine)

import Control.Monad (forever)

import HelloMVar.Control.Parts
  ( Console(getLine)
  , HasNumber(getNumber, putNumber)
  , Forker(forkSetter)
  )

run :: (Console m, HasNumber m, Forker m) => m ()
run = forever step

step :: (Console m, HasNumber m, Forker m) => m ()
step = do
  _ <- getLine
  number <- getNumber
  forkSetter number
  putNumber (number + 1)
