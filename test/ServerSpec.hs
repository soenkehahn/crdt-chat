{-# LANGUAGE OverloadedStrings #-}

module ServerSpec where

import           Control.Exception (throwIO, ErrorCall(..))
import           Control.Monad.Trans.Except
import           Data.Crdt.TreeVector
import           Network.HTTP.Client
import           Network.Wai.Handler.Warp as Warp
import           Servant.API
import           Servant.Client
import           Servant.Common.Req
import           Test.Hspec

import           Api
import           Server (mkApp)

sync :: Document -> Manager -> BaseUrl -> ClientM Document
sync :<|> _ = client api

spec :: Spec
spec = do
  around withApp $ do
    describe "/api/sync" $ do
      it "returns an empty TreeVector" $ \ port -> do
        try port (sync mempty) `shouldReturn` (mempty :: Document)

      it "allows to send a patch" $ \ port -> do
        let patch = mkPatch nilClient mempty ["foo"]
        synced <- try port (sync patch)
        synced `shouldBe` patch

withApp :: (Port -> IO a) -> IO a
withApp action =
  testWithApplication mkApp $ \ port ->
    action port

try :: Port -> (Manager -> BaseUrl -> ClientM a) -> IO a
try port action = do
  manager <- newManager defaultManagerSettings
  let baseUrl = BaseUrl Http "localhost" port ""
  result <- runExceptT (action manager baseUrl)
  either (throwIO . ErrorCall . show) return result
