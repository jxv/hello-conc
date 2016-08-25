module HelloTVar.Broadcast.Run
  ( run
  ) where

import Data.Text (pack)

import HelloTVar.Broadcast.Parts
  ( Delayer(delay)
  , HasNumber(getNumber)
  , Broadcaster(broadcast)
  )

run :: (Broadcaster m, Delayer m, HasNumber m) => m ()
run = do
  number <- getNumber
  let textNumber = pack (show number)
  broadcast $ "Hello, World! - " `mappend` textNumber
  delay 5
  broadcast $ "Bye, World! - " `mappend`textNumber
