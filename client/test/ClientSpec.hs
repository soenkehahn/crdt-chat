
module ClientSpec where

import           CRDT.TreeVector
import           React.Flux
import           Test.Hspec

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

    context "UserInput" $ do
      it "changes the model" $ do
        let model :: Model
            model = Model mempty
        Model doc <- transform (UserInput "foo") model
        getVector doc `shouldBe` "foo"
