module Main (main) where

import qualified HelloMVar (runIO, main)

main :: IO ()
main = HelloMVar.runIO HelloMVar.main
