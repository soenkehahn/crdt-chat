
module ServerSpec where

import           CRDT.TreeVector
import           Control.Exception (throwIO, ErrorCall(..))
import           Control.Monad.Trans.Except
import           Network.HTTP.Client
import           Network.Wai.Handler.Warp as Warp
import           Servant.API
import           Servant.Client
import           Servant.Common.Req
import           Test.Hspec

import           Api
import           Server (mkApp)

getDocument :: Manager -> BaseUrl ->
  ExceptT ServantError IO (TreeVector Char)
(getDocument :<|> sync) :<|> _ = client api

spec = do
  around withApp $ do
    describe "/" $ do
      it "returns an empty TreeVector" $ \ port -> do
        try port getDocument `shouldReturn` (mempty :: Document)

      it "allows to send a patch" $ \ port -> do
        let patch = mkPatch (Client 0) mempty "foo"
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
