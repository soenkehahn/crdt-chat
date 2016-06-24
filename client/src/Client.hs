{-# LANGUAGE DeriveGeneric #-}
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
import           Data.Crdt.TreeVector.Cursor
import           Data.Crdt.TreeVector.Internal
import           Data.Crdt.TreeVector.Pretty
import           Data.Functor
import           Data.Monoid
import           Data.String
import           Data.String.Conversions
import           Data.Text (Text)
import           GHC.Generics
import           Network.HTTP.Client
import           React.Flux
import           React.Flux.Internal (JSString)
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
    errors :: [Text],
    document :: Document,
    cursor :: Int
  }
  deriving (Eq, Show)

store :: ReactStore Model
store = mkStore initial

initial :: Model
initial = Model [] mempty 0

data Msg
  = Server Server
  | Ui Ui

  | Debug String
  | Error Text
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
    Error msg -> \ (Model errors doc cursor) ->
      return $ Model (errors ++ [msg]) doc cursor

transformServer :: Server -> Model -> IO Model
transformServer = \ case
  Sync -> \ model@(Model _ doc _) -> do
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
        Left err ->
          alterStore store $ Error $ cs $ show err
    return model

  Update new -> \ (Model errors doc index) -> do
    let cursor = toCursor doc index
        newDoc = doc <> new
    return $ Model errors newDoc (fromCursor newDoc cursor)

transformUi :: Ui -> Model -> IO Model
transformUi = \ case
  Enter newMessage -> \ (Model errors oldDoc cursor) -> do
    let oldVector = getVector oldDoc
        newVector = insertAt cursor newMessage oldVector
        newDoc = oldDoc <> mkPatch (Client 0) oldDoc newVector
    return $ Model errors newDoc cursor
  UpArrow -> \ (Model errors doc cursor) ->
    return $ Model errors doc (max 0 (cursor - 1))
  DownArrow -> \ (Model errors doc cursor) ->
    return $ Model errors doc (min (length $ getVector doc) (cursor + 1))

-- * transform utils

insertAt :: Int -> a -> [a] -> [a]
insertAt i e list =
  let (prefix, suffix) = splitAt i list in
  prefix ++ [e] ++ suffix

-- * view

viewPatches :: ReactView ()
viewPatches = defineControllerView "patches app" store $ \ model () -> do
  viewErrors $ errors model
  viewChatMessages model
  viewDebug model

viewErrors :: [Text] -> ReactElementM ViewEventHandler ()
viewErrors = \ case
  [] -> return ()
  errs -> do
    "Some errors were encountered:"
    ul_ [] $ do
      forM_ errs $ \ err -> do
        li_ [] $ do
          fromString $ cs err
    hr_ []

viewChatMessages :: Model -> ReactElementM ViewEventHandler ()
viewChatMessages model = do
  div_ [arrowEvents] $ do
    let elements = zip (Nothing : map Just (getVector (document model))) [0 ..]
    forM_ elements $ \ (message, index) -> do
      forM_ message $ \ m -> do
        text_ $ fromString $ cs m
        br_ []
      when (index == cursor model) $ do
        view viewChatInput () mempty


viewChatInput :: ReactView ()
viewChatInput = defineStatefulView "chat input" "" $ \ (text :: Text) () -> do
  text_ $ fromString ">>> "
  input_ $
    ("value" &= text) :
    (set "autoFocus" "true") :
    (onInput $ \ event _ -> ([], Just $ target event "value")) :
    (onEnter $ \ _ _ text -> ([SomeStoreAction store (Ui $ Enter text)], Just "")) :
    []
  br_ []

viewDebug :: Model -> ReactElementM ViewEventHandler ()
viewDebug model = do
  text_ $ fromString $ show (getVector $ document model)
  br_ []
  text_ $ fromString $ show model
  br_ []
  pre_ $ fromString $ ppTree $ document model
  br_ []

-- * view utils

onEnter :: Monoid handler =>
  (Event -> KeyboardEvent -> handler) -> PropertyOrHandler handler
onEnter action = onKeyDown $ \ event keyboardEvent ->
  case keyCode keyboardEvent of
    13 -> action event keyboardEvent
    _ -> mempty

arrowEvents :: PropertyOrHandler [SomeStoreAction]
arrowEvents = onKeyDown $ \ _ keyboardEvent ->
  maybe [] (\ msg -> [SomeStoreAction store (Ui msg)]) $
  case keyCode keyboardEvent of
   38 -> Just UpArrow
   40 -> Just DownArrow
   _ -> Nothing

set :: JSString -> String -> PropertyOrHandler handler
set name value = property name value
