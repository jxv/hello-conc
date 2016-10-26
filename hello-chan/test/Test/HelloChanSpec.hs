{-# LANGUAGE KindSignatures, RankNTypes #-}

module Test.HelloChanSpec (spec) where

import Prelude hiding (log)
import Control.Monad.Trans.Class (lift)
import Control.Monad.TestFixture
import Control.Monad.TestFixture.TH
import Test.Hspec

import HelloChan

mkFixture "Fixture" [''Interthread, ''Wires]

spec :: Spec
spec = do
  describe "run" $ do
    it "should be" $ do
      True `shouldBe` True
