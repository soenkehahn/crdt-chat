{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module ClientSpec where

import           Control.Monad
import           Data.Crdt.TreeVector
import           Data.Crdt.TreeVector.Internal
import           Data.List
import qualified Data.Map
import           Data.Text (Text)
import           Data.UUID
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
    context "Initialize" $ do
      it "initializes the model with the given username" $ do
        Just new <- transform (Initialize "foo") Nothing
        let Client (_, name) = clientId new
        name `shouldBe` "foo"

  describe "transformJust" $ do
    context "Error" $ do
      it "adds the error to the model to be displayed" $ do
        (errors <$> transformJust (Error "foo") nilModel) `shouldReturn` ["foo"]

  describe "transformServer" $ do
    context "Update" $ do
      it "incorporates updates" $ do
        let update = mkPatch nilClient mempty ["foo"]
        new <- transformServer (Update update) nilModel
        document new `shouldBe` update

  describe "transformUi" $ modifyMaxSize (min 10) $ do
    context "Enter" $ do
      it "changes the model" $ do
        doc <- document <$> transformUi (Enter "foo") nilModel
        getVector doc `shouldBe` ["foo"]

      it "creates a message with the model client id" $ do
        let model = nilModel{ clientId = oneClient }
        doc <- document <$> transformUi (Enter "foo") model
        let [(client, _)] = Data.Map.toList $ treeMap doc
        client `shouldSatisfy` (/= nilClient)

      it "inserts the messages at the cursor position" $ do
        let doc = mkPatch nilClient mempty ["foo", "bar"]
            model = Model [] nilClient doc 1
        new <- document <$> transformUi (Enter "huhu") model
        getVector new `shouldBe` ["foo", "huhu", "bar"]

    context "the cursor" $ do
      it "increases + 1 through DownArrow" $ do
        let doc = mkPatch nilClient mempty ["foo"]
            model = Model [] nilClient doc 0
        cursor <$> transformUi DownArrow model `shouldReturn` 1

      it "decreases - 1 through UpArrow" $ do
        let doc = mkPatch nilClient mempty ["foo"]
            model = Model [] nilClient doc 1
        cursor <$> transformUi UpArrow model `shouldReturn` 0

      context "is never out of bounds" $ do
        it "is never too big" $ do
          forAllModels $ \ model -> do
            cursor model >= 0

        it "is never too small" $ do
          forAllModels $ \ model -> do
            cursor model <= (length (getVector (document model)))

      it "uses the crdt cursor" $ do
        let initialDoc = mkPatch nilClient mempty ["foo"]
            initialModel = Model [] nilClient initialDoc 1
            fromServer = mkPatch oneClient initialDoc ["bar", "foo"]
        newModel <- transformServer (Update fromServer) initialModel
        cursor newModel `shouldBe` 2

      it "increases the cursor when adding a message" $ do
        let doc = mkPatch nilClient mempty ["foo", "bar"]
            model = Model [] nilClient doc 1
        new <- transformUi (Enter "huhu") model
        cursor new `shouldBe` 2

forAllModels :: Testable t => (Model -> t) -> Property
forAllModels prop = property $ \ (messages :: [Msg]) -> ioProperty $ do
  Just model <- foldM (\ m msg -> transform msg m) (Just nilModel) messages
  return $
    counterexample (show model) $
    prop model

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
  edits :: [(Client CId, [Text])] <- arbitrary
  return $ foldl' (\ old (client, new) -> mkPatch client old new) mempty edits

instance Arbitrary cid => Arbitrary (Client cid) where
  arbitrary = Client <$> arbitrary

instance Arbitrary UUID where
  arbitrary = choose (nil, nil)

nilModel :: Model
nilModel = Model [] nilClient mempty 0

oneClient :: Client CId
oneClient = case fromString "3201bc50-7e5c-4f92-be27-22bf429f8443" of
  Just uuid -> Client (uuid, "oneClient")
  Nothing -> error "tests: invalid static UUID"
