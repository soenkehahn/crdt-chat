{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Client where

import           Control.Concurrent
import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.Trans.Except
import           Data.Crdt.TreeVector
import           Data.Crdt.TreeVector.Internal
import           Data.Functor
import           Data.Monoid
import           Data.String
import           Data.String.Conversions
import           Data.Text (Text)
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
  alterStore store (Server Sync)
  reactRender "main" viewPatches ()

data Model
  = Model {
    document :: Document,
    cursor :: Int
  }
  deriving (Eq, Show)

store :: ReactStore Model
store = mkStore initial

initial :: Model
initial = Model mempty 0

data Msg
  = Server Server
  | Ui Ui

  | Debug String
  deriving (Generic, Show)

data Server
  = Update Document
  | Sync
  deriving (Generic, Show)

data Ui
  = Enter Text
  | UpArrow
  | DownArrow
  deriving (Generic, Show)

instance NFData Msg
instance NFData Ui
instance NFData Server
instance NFData a => NFData (TreeVector a)
instance NFData a => NFData (Node a)
instance NFData a => NFData (Element a)
instance NFData Data.Crdt.TreeVector.Client

instance StoreData Model where
  type StoreAction Model = Msg
  transform = \ case
    Server msg -> transformServer msg
    Ui msg -> transformUi msg
    Debug msg -> \ model -> putStrLn msg $> model

transformServer :: Server -> Model -> IO Model
transformServer = \ case
  Sync -> \ model@(Model doc _) -> do
    _ <- forkIO $ do
      putStrLn "syncing..."
      baseUrl <- sameOriginBaseUrl Nothing
      result <- runExceptT $
        sync doc (error "manager shouldn't be touched") baseUrl
      putStrLn "received sync data..."
      _ <- forkIO (threadDelay 10000000 >> alterStore store (Server Sync))
      case result of
        Right new ->
          alterStore store $ Server $ Update new
    return model

  Update new -> \ (Model doc cursor) -> do
    return $ Model (doc <> new) cursor

transformUi :: Ui -> Model -> IO Model
transformUi = \ case
  Enter new -> \ (Model old cursor) -> do
    return $ Model
      (old <> mkPatch (Client 0) old (getVector old ++ [new]))
      cursor
  UpArrow -> \ (Model doc cursor) ->
    return $ Model doc (max 0 (cursor - 1))
  DownArrow -> \ (Model doc cursor) ->
    return $ Model doc (min (length $ getVector doc) (cursor + 1))

viewPatches :: ReactView ()
viewPatches = defineControllerView "patches app" store $ \ model () -> do
  div_ [arrowEvents] $ do

    let elements = zip (Nothing : map Just (getVector (document model))) [0 ..]
    forM_ elements $ \ (message, index) -> do
      forM_ message $ \ m -> do
        text_ $ fromString $ cs m
        br_ []
      when (index == cursor model) $ do
        hr_ []

    view chatInput () mempty
    br_ []
    text_ $ fromString $ show (getVector $ document model)
    br_ []
    text_ $ fromString $ show model
    br_ []

chatInput :: ReactView ()
chatInput = defineStatefulView "chat input" "" $ \ (text :: Text) () -> do
  input_ $
    ("value" &= text) :
    (onInput $ \ event _ -> ([], Just $ target event "value")) :
    (onEnter $ \ _ _ text -> ([SomeStoreAction store (Ui $ Enter text)], Just "")) :
    []

-- * view utils

onEnter :: Monoid handler =>
  (Event -> KeyboardEvent -> handler) -> PropertyOrHandler handler
onEnter action = onKeyDown $ \ event keyboardEvent ->
  case keyCode keyboardEvent of
    13 -> action event keyboardEvent
    _ -> mempty

textfield_ :: [PropertyOrHandler eventHandler] -> ReactElementM eventHandler ()
textfield_ props = textarea_ props (return ())

arrowEvents :: PropertyOrHandler [SomeStoreAction]
arrowEvents = onKeyDown $ \ _ keyboardEvent ->
  maybe [] (\ msg -> [SomeStoreAction store (Ui msg)]) $
  case keyCode keyboardEvent of
   38 -> Just UpArrow
   40 -> Just DownArrow
   _ -> Nothing
