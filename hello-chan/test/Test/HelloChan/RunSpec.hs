module Test.HelloChan.RunSpec (spec) where

import Prelude hiding (log)
import Control.Monad.Trans.Class (lift)
import Control.Monad.TestFixture
import Control.Monad.TestFixture.TH
import Test.Hspec

import HelloChan.Run (run)
import HelloChan.Interthread (Interthread)
import HelloChan.Subsystem (Control, Broadcast, Printy)

mkFixture "Fixture" [''Control, ''Broadcast, ''Printy] -- `Interthread` breaks `mkFixture`

spec :: Spec
spec = do
  describe "run" $ do
    it "should be" $ do
      True `shouldBe` True
