{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Client where

import           Control.Concurrent
import           Control.Monad.Trans.Except
import           Data.Monoid
import           Data.String
import           Network.HTTP.Client
import           React.Flux
import           Servant.API
import           Servant.Client
import           Servant.Common.Req

import           Api
import           SameOrigin

sync :: Document -> Manager -> BaseUrl -> ClientM Document
(_ :<|> sync) :<|> _ = client api

run :: IO ()
run = do
  alterStore store Sync
  reactRender "main" viewPatches ()

data Model
  = Model {
    document :: Document
  }
  deriving (Eq, Show)

store :: ReactStore Model
store = mkStore (Model mempty)

data Msg
  = Update Document
  | Sync

instance StoreData Model where
  type StoreAction Model = Msg
  transform msg (Model doc) = case msg of
    Update new -> return $ Model (doc <> new)
    Sync -> do
      _ <- forkIO $ do
        baseUrl <- sameOriginBaseUrl Nothing
        result <- runExceptT $
          sync doc (error "manager shouldn't be touched") baseUrl
        case result of
          Right new ->
            alterStore store $ Update new
      return $ Model doc

viewPatches :: ReactView ()
viewPatches = defineControllerView "patches app" store $ \ doc () -> do
  text_ (fromString (show (document doc)))
  text_ "bla"
