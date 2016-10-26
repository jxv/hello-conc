module Main (main) where

import qualified  HelloChan as HelloChan (main, runIO)

main :: IO ()
main = HelloChan.runIO HelloChan.main
