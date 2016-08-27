module HelloMVar.Printy.Run
  ( run
  , step
  ) where

import Control.Monad (forever)
import Data.Text (pack)

import HelloMVar.Printy.Types (Seconds(..))
import HelloMVar.Printy.Parts
  ( Console(stdout)
  , Receiver(receiveValue)
  , Delayer(delay)
  )

run :: (Console m, Receiver m, Delayer m) => m ()
run = forever step

step :: (Console m, Receiver m, Delayer m) => m ()
step = do
  val <- receiveValue
  stdout . pack . show $ val
  delay (Seconds 1)
