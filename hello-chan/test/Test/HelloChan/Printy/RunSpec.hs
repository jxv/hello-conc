module Test.HelloChan.Printy.RunSpec (spec) where

import Prelude hiding (log)
import Control.Monad.Trans.Class (lift)
import Control.Monad.TestFixture
import Control.Monad.TestFixture.TH
import Test.Hspec

import HelloChan.Printy.Run (step)
import HelloChan.Printy.Parts (Console, Receiver)


mkFixture "Fixture" [''Console, ''Receiver]

spec :: Spec
spec = do
  describe "step" $ do
    it "should call stdout with received message" $ do
      let stubMessage = "MESSAGE"
      let fixture = def
            { _receiveMessage = return stubMessage
            , _stdout = \msg -> do
                log "stdout"
                lift $ msg `shouldBe` stubMessage
            }
      captured <- logTestFixtureT step fixture
      captured `shouldBe` ["stdout"]
