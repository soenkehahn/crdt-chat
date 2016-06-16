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
import           Data.Foldable
import           Data.Functor
import           Data.Monoid
import           Data.String
import           Data.String.Conversions
import           Data.Text
import           GHC.Generics
import           Network.HTTP.Client
import           React.Flux
import           Servant.API
import           Servant.Client
import           Servant.Common.Req

import           Api
import           SameOrigin

sync :: Document -> Manager -> BaseUrl -> ClientM Document
sync :<|> _ = client api

run :: IO ()
run = do
  alterStore store Sync
  reactRender "main" viewPatches ()

data Model
  = Model {
    document :: Document,
    inputField :: Text
  }
  deriving (Eq, Show)

store :: ReactStore Model
store = mkStore (Model mempty "")

data Msg
  = Update Document
  | Sync
  | Input Text
  | Enter

  | Debug String
  deriving (Generic)

instance NFData Msg
instance NFData a => NFData (TreeVector a)
instance NFData a => NFData (Node a)
instance NFData a => NFData (Element a)
instance NFData CRDT.TreeVector.Client

instance StoreData Model where
  type StoreAction Model = Msg
  transform msg (Model doc inputField) = case msg of
    Update new -> return $ Model (doc <> new) inputField
    Sync -> do
      _ <- forkIO $ do
        baseUrl <- sameOriginBaseUrl Nothing
        result <- runExceptT $
          sync doc (error "manager shouldn't be touched") baseUrl
        _ <- forkIO (threadDelay 1000000 >> alterStore store Sync)
        case result of
          Right new ->
            alterStore store $ Update new
      return $ Model doc inputField

    Input s -> return $ Model doc s
    Enter -> return $ Model
      (mkPatch (Client 0) doc (getVector doc ++ [inputField]))
      ""
    Debug msg -> putStrLn msg $> Model doc inputField

viewPatches :: ReactView ()
viewPatches = defineControllerView "patches app" store $ \ model () -> do
  ul_ [] $ do
    forM_ (getVector (document model)) $ \ message -> do
      li_ [] $ do
        text_ $ fromString $ cs message
  input_ $
    "value" &= inputField model :
    (onInput $ \ event -> [SomeStoreAction store (Input (target event "value"))]) :
    (onEnter $ \ _ _ -> [SomeStoreAction store Enter]) :
    []
  br_ []
  text_ $ fromString $ show (getVector $ document model)
  br_ []
  text_ $ fromString $ show (document model)

onEnter :: (Event -> KeyboardEvent -> [SomeStoreAction]) -> PropertyOrHandler [SomeStoreAction]
onEnter action = onKeyDown $ \ event keyboardEvent ->
  case keyCode keyboardEvent of
    13 -> action event keyboardEvent
    _ -> []
