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
import           Data.Crdt.TreeVector as Crdt
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
import           System.Random

import           Api
import           SameOrigin

sync :: Document -> Manager -> BaseUrl -> ClientM Document
sync :<|> _ = client api

run :: IO ()
run = do
  reactRender "main" viewPatches ()

data Model
  = Model {
    errors :: [Text],
    clientId :: Crdt.Client CId,
    document :: Document,
    cursor :: Int
  }
  deriving (Eq, Show)

store :: ReactStore (Maybe Model)
store = mkStore Nothing

mkInitial :: Text -> IO Model
mkInitial userName = do
  uuid <- randomIO
  return $ Model [] (Client (uuid, userName)) mempty 0

data Msg
  = Initialize Text
  | Server Server
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
instance NFData a => NFData (TreeVector CId a)
instance NFData a => NFData (Node CId a)
instance NFData a => NFData (Element a)
instance NFData (Crdt.Client CId)

instance StoreData (Maybe Model) where
  type StoreAction (Maybe Model) = Msg
  transform msg mModel = case (msg, mModel) of
    (Initialize userName, Nothing) -> do
      _ <- forkIO $ do
        alterStore store (Server Sync)
      Just <$> mkInitial userName
    (msg, Nothing) -> do
      putStrLn ("model not initialized, discarding message: " ++ show msg)
      return Nothing
    (_, Just model) -> Just <$> transformJust msg model

transformJust :: Msg -> Model -> IO Model
transformJust = \ case
  Initialize _ -> return
  Server msg -> transformServer msg
  Ui msg -> transformUi msg
  Debug msg -> \ model -> putStrLn msg $> model
  Error msg -> \ (Model errors cid doc cursor) ->
    return $ Model (errors ++ [msg]) cid doc cursor

transformServer :: Server -> Model -> IO Model
transformServer = \ case
  Sync -> \ model@(Model _ _ doc _) -> do
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

  Update new -> \ (Model errors cid doc index) -> do
    let cursor = toCursor doc index
        newDoc = doc <> new
    return $ Model errors cid newDoc (fromCursor newDoc cursor)

transformUi :: Ui -> Model -> IO Model
transformUi = \ case
  Enter newMessage -> \ (Model errors cid oldDoc cursor) -> do
    let oldVector = getVector oldDoc
        newVector = insertAt cursor newMessage oldVector
        newDoc = oldDoc <> mkPatch cid oldDoc newVector
    return $ Model errors cid newDoc (succ cursor)
  UpArrow -> \ (Model errors cid doc cursor) ->
    return $ Model errors cid doc (max 0 (cursor - 1))
  DownArrow -> \ (Model errors cid doc cursor) ->
    return $ Model errors cid doc (min (length $ getVector doc) (cursor + 1))

-- * transform utils

insertAt :: Int -> a -> [a] -> [a]
insertAt i e list =
  let (prefix, suffix) = splitAt i list in
  prefix ++ [e] ++ suffix

-- * view

viewPatches :: ReactView ()
viewPatches = defineControllerView "patches app" store $ \ mModel () ->
  case mModel of
    Nothing -> center $ do
      view (focusedInput "name:" Initialize) () mempty
    Just model -> center $ do
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
    let elements = zip (Nothing : map Just (getVectorWithClients (document model))) [0 ..]
    forM_ elements $ \ (message, index) -> do
      forM_ message $ \ (Client (_, userName), m) -> do
        fromString $ cs userName
        ": "
        text_ $ fromString $ cs m
        br_ []
      when (index == cursor model) $ do
        view (focusedInput ">>>" (Ui . Enter)) () mempty

viewDebug :: Model -> ReactElementM ViewEventHandler ()
viewDebug model = do
  hr_ []
  h4_ [] "debugging:"
  pre_ $ fromString $ ppTree $ document model
  br_ []
  text_ $ fromString $ show (getVector $ document model)
  br_ []
  text_ $ fromString $ show model
  br_ []

-- * view utils

focusedInput :: Text -> (Text -> Msg) -> ReactView ()
focusedInput label f = defineStatefulView "chat input" "" $ \ (text :: Text) () -> do
  text_ [] (fromString $ cs label)
  elemText " "
  input_ $
    ("value" &= text) :
    (set "autoFocus" "true") :
    (onInput $ \ event _ -> ([], Just $ target event "value")) :
    (onEnter $ \ _ _ text -> ([SomeStoreAction store (f text)], Just "")) :
    []
  br_ []

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

center :: ReactElementM v p -> ReactElementM v p
center child = do
  div_ [style [("width", "100%"), ("position", "absolute")]] $ do
    div_ [style [("width", "600px"), ("margin", "auto")]] $ do
      child
