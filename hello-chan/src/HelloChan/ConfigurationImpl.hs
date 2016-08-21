module HelloChan.ConfigurationImpl
  ( target
  ) where

import Data.Text (Text)

import HelloChan.Console (Console(sysArg))

target :: Console m => m Text
target = sysArg
