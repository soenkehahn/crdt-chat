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

  describe "transform Model" $ do
    context "Error" $ do
      it "adds the error to the model to be displayed" $ do
        (errors <$> transform (Error "foo") (Model [] Empty)) `shouldReturn` ["foo"]

    context "SetUserName" $ do
      it "initializes the model with the given username" $ do
        transform (SetUserName "foo") (Model [] Empty) `shouldReturn`
          Model [] (WithUserName "foo")

    context "SetChatId" $ do
      it "starts the chat" $ do
        Model [] (StateChatting new) <-
          return (Model [] Empty) >>=
          transform (SetUserName "user") >>=
          transform (SetChatId oneChatId)
        chatId new `shouldBe` oneChatId

  describe "transform Chatting" $ do
    context "Update" $ do
      it "incorporates updates" $ do
        let update = mkPatch nilClient mempty ["foo"]
        new <- transform (Update update) nilChatting
        document new `shouldBe` update

  modifyMaxSize (min 10) $ do
    context "Enter" $ do
      it "changes the model" $ do
        doc <- document <$> transform (Enter "foo") nilChatting
        getVector doc `shouldBe` ["foo"]

      it "creates a message with the model client id" $ do
        let model = nilChatting{ clientId = oneClient }
        doc <- document <$> transform (Enter "foo") model
        let [(client, _)] = Data.Map.toList $ treeMap doc
        client `shouldSatisfy` (/= nilClient)

      it "inserts the messages at the cursor position" $ do
        let doc = mkPatch nilClient mempty ["foo", "bar"]
            state = Chatting nilClient oneChatId doc 1
        new <- document <$> transform (Enter "huhu") state
        getVector new `shouldBe` ["foo", "huhu", "bar"]

    context "the cursor" $ do
      it "increases + 1 through DownArrow" $ do
        let doc = mkPatch nilClient mempty ["foo"]
            state = Chatting nilClient oneChatId doc 0
        cursor <$> transform DownArrow state `shouldReturn` 1

      it "decreases - 1 through UpArrow" $ do
        let doc = mkPatch nilClient mempty ["foo"]
            state = Chatting nilClient oneChatId doc 1
        cursor <$> transform UpArrow state `shouldReturn` 0

      context "is never out of bounds" $ do
        it "is never too big" $ do
          forAllChatting $ \ state -> do
            cursor state >= 0

        it "is never too small" $ do
          forAllChatting $ \ state -> do
            cursor state <= (length (getVector (document state)))

      it "uses the crdt cursor" $ do
        let initialDoc = mkPatch nilClient mempty ["foo"]
            initialState = Chatting nilClient oneChatId initialDoc 1
            fromServer = mkPatch oneClient initialDoc ["bar", "foo"]
        newModel <- transform (Update fromServer) initialState
        cursor newModel `shouldBe` 2

      it "increases the cursor when adding a message" $ do
        let doc = mkPatch nilClient mempty ["foo", "bar"]
            state = Chatting nilClient oneChatId doc 1
        new <- transform (Enter "huhu") state
        cursor new `shouldBe` 2

      it "increases the cursor when someone else adds a message to the same thread" $ do
        pending

forAllChatting :: Testable t => (Chatting -> t) -> Property
forAllChatting prop = property $ \ (messages :: [ChattingMsg]) -> ioProperty $ do
  state <- foldM (\ m msg -> transform msg m) nilChatting messages
  return $
    counterexample (show state) $
    prop state

instance Arbitrary ChattingMsg where
  arbitrary = oneof $
    (Update <$> arbitraryDoc) :
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

nilChatting :: Chatting
nilChatting = Chatting nilClient oneChatId mempty 0

oneClient :: Client CId
oneClient = case fromString "3201bc50-7e5c-4f92-be27-22bf429f8443" of
  Just uuid -> Client (uuid, "oneClient")
  Nothing -> error "tests: invalid static UUID"

oneChatId :: ChatId
oneChatId = case fromString "6306bc50-7e5c-4f92-be27-22bf429f8443" of
  Just uuid -> ChatId uuid
  Nothing -> error "tests: invalid static UUID"
