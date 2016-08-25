module Test.HelloChan.Main.RunSpec (spec) where

import Prelude hiding (log)
import Control.Monad.Trans.Class (lift)
import Control.Monad.TestFixture
import Control.Monad.TestFixture.TH
import Test.Hspec

import HelloChan.Main.Run (run)
import HelloChan.Main.Parts (Interthread, Control, Broadcast, Printy)

mkFixture "Fixture" [''Control, ''Broadcast, ''Printy] -- `newChan` from `Interthread` breaks `mkFixture`

spec :: Spec
spec = do
  describe "run" $ do
    it "should be" $ do
      True `shouldBe` True
