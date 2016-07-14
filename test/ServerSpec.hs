{-# LANGUAGE OverloadedStrings #-}

module ServerSpec where

import           Control.Exception (throwIO, ErrorCall(..))
import           Control.Monad.Trans.Except
import           Data.Crdt.TreeVector
import           Data.UUID
import           Network.HTTP.Client hiding (responseStatus)
import           Network.Wai.Handler.Warp as Warp
import           Servant.API hiding (Header)
import           Servant.Client hiding (responseBody)
import           Test.Hspec

import           Api
import           Server (mkApp)

new :: Manager -> BaseUrl -> ClientM ChatId
sync :: ChatId -> Document -> Manager -> BaseUrl -> ClientM Document
new :<|> sync :<|> _ = client api

spec :: Spec
spec = do
  around withApp $ do
    describe "/api/new" $ do
      it "creates a new chatId" $ \ port -> do
        chatId <- try port new
        chatId `shouldNotBe` nilChatId

    describe "/api/:chat-id" $ do
      it "returns an empty TreeVector" $ \ port -> do
        chatId <- try port new
        try port (sync chatId mempty) `shouldReturn` (mempty :: Document)

    describe "/api/:chat-id/sync" $ do
      it "allows to send a patch" $ \ port -> do
        chatId <- try port new
        let patch = mkPatch nilClient mempty ["foo"]
        synced <- try port (sync chatId patch)
        synced `shouldBe` patch

withApp :: (Port -> IO a) -> IO a
withApp action =
  testWithApplication mkApp $ \ port ->
    action port

try :: Port -> (Manager -> BaseUrl -> ClientM a) -> IO a
try port action = do
  manager <- newManager defaultManagerSettings{
    managerModifyRequest = \ r -> do
      return $ noRedirection r
  }
  let baseUrl = BaseUrl Http "localhost" port ""
  runExceptT (action manager baseUrl) >>=
    either (throwIO . ErrorCall . show) return

noRedirection :: Request -> Request
noRedirection r = r{
  redirectCount = 0
}

oneChatId :: ChatId
oneChatId = case fromString "6306bc50-7e5c-4f92-be27-22bf429f8443" of
  Just uuid -> ChatId uuid
  Nothing -> error "tests: invalid static UUID"
