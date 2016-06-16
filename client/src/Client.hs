{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Client where

import           CRDT.TreeVector
import           CRDT.TreeVector.Internal
import           Control.Concurrent
import           Control.DeepSeq
import           Control.Monad.Trans.Except
import           Data.Monoid
import           Data.String
import           GHC.Generics
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
  | UserInput String
  deriving (Generic)

instance NFData Msg
instance NFData (TreeVector Char)
instance NFData (Node Char)
instance NFData (Element Char)
instance NFData CRDT.TreeVector.Client

instance StoreData Model where
  type StoreAction Model = Msg
  transform msg (Model doc) = case msg of
    Update new -> return $ Model (doc <> new)
    Sync -> do
      _ <- forkIO $ do
        baseUrl <- sameOriginBaseUrl Nothing
        result <- runExceptT $
          sync doc (error "manager shouldn't be touched") baseUrl
        _ <- forkIO (threadDelay 1000000 >> alterStore store Sync)
        case result of
          Right new ->
            alterStore store $ Update new
      return $ Model doc

    UserInput s -> return (Model (doc <> mkPatch (Client 0) doc s))

viewPatches :: ReactView ()
viewPatches = defineControllerView "patches app" store $ \ model () -> do
  textarea_
    ("value" &= getVector (document model) :
     (onChange $ \ event -> [SomeStoreAction store (UserInput (target event "value"))]) :
     []) mempty
  br_ []
  text_ $ fromString $ show (getVector $ document model)
  br_ []
  text_ $ fromString $ show (document model)
