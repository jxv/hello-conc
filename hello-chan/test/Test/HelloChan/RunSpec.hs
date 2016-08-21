module Test.HelloChan.RunSpec (spec) where

import Prelude hiding (log)
import Control.Monad.Trans.Class (lift)
import Control.Monad.TestFixture
import Control.Monad.TestFixture.TH
import Test.Hspec

import HelloChan.Run (run)
import HelloChan.Configuration (Configuration(..))

mkFixture "Fixture" [''Configuration]

spec :: Spec
spec = do
  describe "run" $ do
    it "should be" $ do
      let fixture = def :: FixturePure
      let () = unTestFixture run fixture
      True `shouldBe` True
