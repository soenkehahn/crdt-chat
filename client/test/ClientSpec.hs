{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module ClientSpec where

import           Control.Exception
import           Control.Monad
import           Control.Monad.Operational.Mocks
import           Data.Crdt.TreeVector
import           Data.Crdt.TreeVector.Internal
import           Data.List
import qualified Data.Map
import           Data.Text (Text)
import           Data.Typeable
import           Data.UUID
import           React.Flux (transform)
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()
import           Unsafe.Coerce

import           Api
import           Client
import           Primitive

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
        testWithMock (transformState (SetUserName "foo") Empty) $
          GetHashFragment `returns` (Right Nothing) `andThen`
          testPrimitive isFork () `andThen`
          result (WithUserName "foo")

      it "asks the server to create a new chat" $ do
        let asksForNewChat =
              NewChat `returns` (Right oneChatId) `andThen`
              isAlterStore (SetChatId oneChatId) `andThen`
              result ()
        testWithMock (transformState (SetUserName "user") Empty) $
          GetHashFragment `returns` (Right Nothing) `andThen`
          testPrimitive (isForkMocked asksForNewChat) () `andThen`
          result (WithUserName "user")

      it "takes the chat-id from the address bar, if available" $ do
        testWithMock (transformState (SetUserName "user") Empty) $
          GetHashFragment `returns` (Right $ Just $ ChatId oneUuid) `andThen`
          testPrimitive (isForkMocked syncMock) () `andThen`
          NewUuid `returns` twoUuid `andThen`
          SetHashFragment (ChatId oneUuid) `returns` () `andThen` -- fixme: remove?
          result (StateChatting $ Chatting
            (Client (twoUuid, "user")) (ChatId oneUuid) mempty 0)

    context "SetChatId" $ do
      it "starts the chat" $ do
        testWithMock (transformState (SetChatId oneChatId) (WithUserName "user")) $
          testPrimitive isFork () `andThen`
          NewUuid `returns` twoUuid `andThen`
          SetHashFragment oneChatId `returns` () `andThen`
          result (StateChatting
            (Chatting (Client (twoUuid, "user")) oneChatId mempty 0))

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
        case Data.Map.toList $ treeMap doc of
          [(client, _)] -> do
            client `shouldSatisfy` (/= nilClient)
          _ -> error "no match"

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
oneClient = case fromString "3201bc50-7e5c-4f92-be27-000000000000" of
  Just uuid -> Client (uuid, "oneClient")
  Nothing -> error "tests: invalid static UUID"

oneChatId :: ChatId
oneChatId = case fromString "6306bc50-7e5c-4f92-be27-aaaaaaaaaaaa" of
  Just uuid -> ChatId uuid
  Nothing -> error "tests: invalid static UUID"

oneUuid :: UUID
oneUuid = case fromString "6306bc50-7e5c-4f92-be27-ffffffffffff" of
  Just uuid -> uuid
  Nothing -> error "tests: invalid static UUID"

twoUuid :: UUID
twoUuid = case fromString "6306bc50-7e5c-4f92-be27-bbbbbbbbbbbb" of
  Just uuid -> uuid
  Nothing -> error "tests: invalid static UUID"

-- * CommandEq

isFork :: Primitive a -> IO (a :~: ())
isFork = \ case
  Fork _ -> return Refl
  _ -> throwIO $ ErrorCall "isFork: no Fork"

isForkMocked :: Mock Primitive () -> Primitive a -> IO (a :~: ())
isForkMocked mock = \ case
  Fork realForked -> do
    testWithMock realForked mock
    return Refl
  _ -> throwIO $ ErrorCall "isFork: no Fork"

syncMock :: Mock Primitive ()
syncMock =
  isAlterStore (ChattingMsg Sync) `andThen`
  result ()

isAlterStore :: (Show msg, Eq msg, Typeable msg) =>
  msg -> MockedPrimitive Primitive ()
isAlterStore mock = testPrimitive p ()
  where
    p :: Primitive a -> IO (a :~: ())
    p = \ case
      AlterStore _ msg -> case cast msg of
        Just msg' -> do
          msg' `shouldBe` mock
          return Refl
        Nothing -> throwIO $ ErrorCall "isAlterStore: incorrect type"
      _ -> throwIO $ ErrorCall "isAlterStore: no AlterStore"

instance CommandEq Primitive where
  commandEq a b = case (a, b) of
    (IO a, IO b) -> case cast a of
      Nothing -> error "not the same type"
      Just a' -> return $ Right $ const (unsafeCoerce Refl) (asTypeOf b a')
    (IO _, _) -> return $ Left ()

    (Fork _, Fork _) -> error "Fork here"
    (Fork _, _) -> error "Fork should be tested with 'testPrimitive'"

    (GetHashFragment, GetHashFragment) -> return $ Right Refl
    (GetHashFragment, _) -> return $ Left ()
    (SetHashFragment a, SetHashFragment b) -> do
      a `shouldBe` b
      return $ Right Refl

    (NewUuid, NewUuid) -> return $ Right Refl
    (NewUuid, _) -> return $ Left ()
    (NewChat, NewChat) -> return $ Right Refl

    (a, _) -> error ("commandEq: " ++ showConstructor a)

instance ShowConstructor Primitive where
  showConstructor = \ case
    IO _ -> "IO"
    Fork _ -> "Fork"

    GetHashFragment -> "GetHashFragment"
    SetHashFragment _ -> "SetHashFragment"
    NewUuid -> "NewUuid"
    NewChat -> "NewChat"
    AlterStore _ _ -> "AlterStore"
