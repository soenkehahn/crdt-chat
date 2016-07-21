{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.Wai.Application.CrdtChat (mkApp) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           Network.Wai
import           Network.Wai.MakeAssets
import           Servant
import           System.Directory

import           Api
import qualified Db
import           Db hiding (newChat)

mkApp :: IO Application
mkApp = do
  assets :: Application <- $(do
        exists <- runIO $ doesFileExist "ENVIRONMENT"
        when (not exists) $
          runIO $ writeFile "ENVIRONMENT" "PRODUCTION"
        env <- runIO $ readFile "ENVIRONMENT"
        addDependentFile "ENVIRONMENT"
        case words env of
          ["DEVELOPMENT"] -> [|serveAssets def|]
          ["PRODUCTION"] -> serveAssetsEmbedded def)
  db <- newDb
  return $ serve api (server assets db)

server :: Application -> Db -> Server Api
server assets db =
  newChat db :<|>
  chatSync db :<|>
  assets

newChat :: Db -> Handler ChatId
newChat db = liftIO $ Db.newChat db

chatSync :: Db -> ChatId -> Document -> Handler Document
chatSync db chatId new = do
  mDoc <- liftIO $ Db.sync db chatId new
  maybe (throwError err404) return mDoc
