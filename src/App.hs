
module App where

import           Control.Monad.Trans.Except
import           Network.Wai
import           Network.Wai.MakeAssets
import           Servant

import           Api

mkApp :: IO Application
mkApp = do
  assets <- serveAssets
  return $ serve api (server assets)

server :: Application -> Server Api
server assets =
  getDocument :<|>
  assets

type Handler = ExceptT ServantErr IO

getDocument :: Handler Document
getDocument = return mempty
