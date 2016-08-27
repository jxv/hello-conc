module HelloMVar.Setter.Run
  ( run
  ) where

import Data.Text (pack)
import Data.Functor (void)

import HelloMVar.Setter.Parts
  ( HasNumber(getNumber)
  , Set(set)
  )

run :: (Set m, HasNumber m) => m ()
run = do
  number <- getNumber
  set number
