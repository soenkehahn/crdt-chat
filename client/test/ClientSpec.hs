
module ClientSpec where

import           React.Flux
import           Test.Hspec
import           Test.Mockery.Directory

spec = do
  describe "transform" $ do
    context "ButtonSync" $ do
      it "sends a sync event" $ do
        inTempDirectory $ do
          pending

    context "Update" $ do
      it "incorporates updates" $ do
        pending
