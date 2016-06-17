{-# LANGUAGE OverloadedStrings #-}

module ClientSpec where

import           Data.Crdt.TreeVector
import           React.Flux
import           Test.Hspec

import           Client

spec :: Spec
spec = do
  describe "transform" $ do
    context "Update" $ do
      it "incorporates updates" $ do
        let model :: Model
            model = Model mempty ""
            update = mkPatch (Client 0) mempty ["foo"]
        transform (Update update) model `shouldReturn` Model update ""

    context "Input" $ do
      it "changes the model" $ do
        let model :: Model
            model = Model mempty ""
        Model _ text <- transform (Input "foo") model
        text `shouldBe` "foo"
