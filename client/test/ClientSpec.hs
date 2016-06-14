
module ClientSpec where

import           CRDT.TreeVector
import           React.Flux
import           Test.Hspec
import           Test.Mockery.Directory

import           Client

spec :: Spec
spec = do
  describe "transform" $ do
    context "Update" $ do
      it "incorporates updates" $ do
        let model :: Model
            model = Model mempty
            update = mkPatch (Client 0) mempty "foo"
        transform (Update update) model `shouldReturn` Model update
