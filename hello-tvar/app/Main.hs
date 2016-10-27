module Main (main) where

import qualified HelloTVar (runIO, main)

main :: IO ()
main = HelloTVar.runIO HelloTVar.main
