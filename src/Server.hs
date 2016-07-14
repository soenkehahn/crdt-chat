
module Server where

import           Control.Monad.IO.Class
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
  Server.newChat db :<|>
  chatSync db :<|>
  assets

newChat :: Db -> Handler ChatId
newChat db = liftIO $ Db.newChat db

chatSync :: Db -> ChatId -> Document -> Handler Document
chatSync db chatId new = do
  mDoc <- liftIO $ Db.sync db chatId new
  maybe (throwError err404) return mDoc
