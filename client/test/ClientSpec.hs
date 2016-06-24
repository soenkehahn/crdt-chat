{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module ClientSpec where

import           Control.Monad
import           Data.Crdt.TreeVector
import           Data.List
import           Data.Text (Text)
import           React.Flux (transform)
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           Api
import           Client

spec :: Spec
spec = do
  describe "insertAt" $ do
    it "inserts at the given index" $ do
      insertAt 1 'x' "ab" `shouldBe` "axb"

  describe "transform" $ do
    context "Error" $ do
      it "adds the error to the model to be displayed" $ do
        (errors <$> transform (Error "foo") initial) `shouldReturn` ["foo"]

  describe "transformServer" $ do
    context "Update" $ do
      it "incorporates updates" $ do
        let update = mkPatch (Client 0) mempty ["foo"]
        new <- transformServer (Update update) initial
        document new `shouldBe` update

  describe "transformUi" $ modifyMaxSize (min 10) $ do
    context "Enter" $ do
      it "changes the model" $ do
        doc <- document <$> transformUi (Enter "foo") initial
        getVector doc `shouldBe` ["foo"]

      it "inserts the messages at the cursor position" $ do
        let doc = mkPatch (Client 0) mempty ["foo", "bar"]
            model = Model [] doc 1
        new <- document <$> transformUi (Enter "huhu") model
        getVector new `shouldBe` ["foo", "huhu", "bar"]

    context "the cursor" $ do
      it "increases + 1 through DownArrow" $ do
        let doc = mkPatch (Client 0) mempty ["foo"]
            model = Model [] doc 0
        cursor <$> transformUi DownArrow model `shouldReturn` 1

      it "decreases - 1 through UpArrow" $ do
        let doc = mkPatch (Client 0) mempty ["foo"]
            model = Model [] doc 1
        cursor <$> transformUi UpArrow model `shouldReturn` 0

      context "is never out of bounds" $ do
        it "is never too big" $ do
          forAllModels $ \ model -> do
            cursor model >= 0

        it "is never too small" $ do
          forAllModels $ \ model -> do
            cursor model <= (length (getVector (document model)))

      it "uses the crdt cursor" $ do
        let initialDoc = mkPatch (Client 0) mempty ["foo"]
            initialModel = Model [] initialDoc 1
            fromServer = mkPatch (Client 1) initialDoc ["bar", "foo"]
        newModel <- transformServer (Update fromServer) initialModel
        cursor newModel `shouldBe` 2

      it "increases the cursor when adding a message" $ do
        let doc = mkPatch (Client 0) mempty ["foo", "bar"]
            model = Model [] doc 1
        new <- transformUi (Enter "huhu") model
        cursor new `shouldBe` 2

forAllModels :: Testable t => (Model -> t) -> Property
forAllModels cont = property $ \ (messages :: [Msg]) -> ioProperty $ do
  model <- foldM (\ m msg -> transform msg m) initial messages
  return $
    counterexample (show model) $
    cont model

instance Arbitrary Msg where
  arbitrary = oneof $
    (Server <$> arbitrary) :
    (Ui <$> arbitrary) :
--    (Debug <$> arbitrary) :
    []

instance Arbitrary Server where
  arbitrary = oneof $
    (Update <$> arbitraryDoc) :
--    pure Sync :
    []

instance Arbitrary Ui where
  arbitrary = oneof $
    (Enter <$> arbitrary) :
    pure UpArrow :
    pure DownArrow :
    []

arbitraryDoc :: Gen Document
arbitraryDoc = do
  edits :: [(Client, [Text])] <- arbitrary
  return $ foldl' (\ old (client, new) -> mkPatch client old new) mempty edits

instance Arbitrary Client where
  arbitrary = Client <$> arbitrary
