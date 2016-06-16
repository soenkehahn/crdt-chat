
module Server where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import           Network.Wai
import           Network.Wai.MakeAssets
import           Servant

import           Api
import           Db

mkApp :: IO Application
mkApp = do
  assets <- serveAssets
  db <- newDb
  return $ serve api (server assets db)

server :: Application -> Db -> Server Api
server assets db =
  Server.sync db :<|>
  assets

type Handler = ExceptT ServantErr IO

sync :: Db -> Document -> Handler Document
sync db new = liftIO $ Db.sync db new
