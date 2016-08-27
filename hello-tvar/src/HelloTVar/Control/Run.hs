module HelloTVar.Control.Run
  ( run
  , step
  ) where

import Prelude hiding (getLine)

import Control.Monad (forever)

import HelloTVar.Control.Parts
  ( Console(getLine)
  , HasNumber(getNumber, putNumber)
  , Forker(forkUpdater)
  )

run :: (Console m, HasNumber m, Forker m) => m ()
run = forever step

step :: (Console m, HasNumber m, Forker m) => m ()
step = do
  _ <- getLine
  number <- getNumber
  forkUpdater number
  putNumber (number + 1)
