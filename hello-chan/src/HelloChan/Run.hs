module HelloChan.Run
  ( run
  ) where

import Data.Text (Text)
import Data.Functor (void)

data Chan m a = Chan
  { _writeChan :: a -> m ()
  , _readChan :: m a
  }

class Monad m => Interfaces m where
  newChan :: m (Chan m a)
  fork :: m () -> m ()
  runControl :: (Int -> m (), Int) -> m ()
  runBroadcast :: (a -> m (), Int) -> m ()
  runPrint :: m Text -> m ()

run :: Interfaces m => m ()
run = do
  chan <- newChan
  fork $ runControl (\number -> fork $ runBroadcast (_writeChan chan, number), 0)
  runPrint (_readChan chan)
