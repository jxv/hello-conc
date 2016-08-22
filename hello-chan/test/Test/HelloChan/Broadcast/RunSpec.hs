module Test.HelloChan.Broadcast.RunSpec (spec) where

import Prelude hiding (log)
import Control.Monad.Trans.Class (lift)
import Control.Monad.TestFixture
import Control.Monad.TestFixture.TH
import Test.Hspec

import HelloChan.Broadcast.Run (run)

spec :: Spec
spec = do
  describe "run" $ do
    it "should be" $ do
      True `shouldBe` True