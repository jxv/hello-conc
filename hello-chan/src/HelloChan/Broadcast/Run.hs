module HelloChan.Broadcast.Run
  ( run
  ) where

import Data.Text (pack)

import HelloChan.Broadcast.Delayer (Delayer(delay))
import HelloChan.Broadcast.HasNumber (HasNumber(getNumber))
import HelloChan.Broadcast.Broadcaster (Broadcaster(broadcast))


run :: (Broadcaster m, Delayer m, HasNumber m) => m ()
run = do
  number <- getNumber
  let textNumber = pack (show number)
  broadcast $ "Hello, World! - " `mappend` textNumber
  delay 5
  broadcast $ "Bye, World! - " `mappend`textNumber
