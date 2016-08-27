module HelloTVar.Setter.Run
  ( run
  ) where

import Data.Text (pack)
import Data.Functor (void)

import HelloTVar.Setter.Parts
  ( HasNumber(getNumber)
  , Set(set)
  )

run :: (Set m, HasNumber m) => m ()
run = do
  number <- getNumber
  set number
