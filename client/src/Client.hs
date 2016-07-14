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

import           Api
import           Primitive
import           SameOrigin

new :: Manager -> BaseUrl -> ClientM ChatId
sync :: ChatId -> Document -> Manager -> BaseUrl -> ClientM Document
new :<|> sync :<|> _ = client api

withApiCall :: ClientCommand (Either ServantError a) -> (a -> ClientCommand ()) -> ClientCommand ()
withApiCall apiCall action = do
  result <- apiCall
  case result of
    Right response -> action response
    Left err -> alterStoreP store $ Error $ cs $ show err

run :: IO ()
run = do
  reactRender "main" viewPatches ()

data Model
  = Model {
    errors :: [Text],
    state :: State
  }
  deriving (Eq, Show)

data State
  = Empty
  | WithUserName Text
  | StateChatting Chatting
  deriving (Eq, Show)

data Chatting
  = Chatting {
    clientId :: Crdt.Client CId,
    chatId :: ChatId,
    document :: Document,
    cursor :: Int
  }
  deriving (Eq, Show)

store :: ReactStore Model
store = mkStore (Model [] Empty)

toChatting :: Text -> ChatId -> ClientCommand Chatting
toChatting userName chatId = do
  uuid <- newUuid
  setHashFragment chatId
  return $ Chatting (Client (uuid, userName)) chatId mempty 0

data Msg
  = Error Text
  | Debug String
  | SetUserName Text
  | SetChatId ChatId
  | ChattingMsg ChattingMsg
  deriving (Generic, Show, Eq)

data ChattingMsg
  = Update Document
  | Sync
  | Enter Text
  | UpArrow
  | DownArrow
  deriving (Generic, Show, Eq)

instance NFData Msg
instance NFData ChattingMsg
instance NFData ChatId
instance NFData a => NFData (TreeVector CId a)
instance NFData a => NFData (Node CId a)
instance NFData a => NFData (Element a)
instance NFData (Crdt.Client CId)

instance StoreData Model where
  type StoreAction Model = Msg
  transform msg model@(Model errs state) = do
   print msg
   case msg of
    Debug msg -> putStrLn msg $> model
    Error msg -> return $ Model (errs ++ [msg]) state
    _ -> Model errs <$> Primitive.run (transformState msg state)

transformState :: Msg -> State -> ClientCommand State
transformState msg state =
    case (msg, state) of
      (SetUserName user, Empty) -> do
        hash <- getHashFragment
        case hash of
          Left err -> do
            fork $ alterStoreP store (Error $ cs err)
            return $ WithUserName user
          Right Nothing -> do
            fork $ do
              withApiCall newChat $ \ chatId ->
                alterStoreP store (SetChatId chatId)
            return $ WithUserName user
          Right (Just chatId) -> do
            _ <- fork $ do
              alterStoreP store (ChattingMsg Sync)
            StateChatting <$> toChatting user chatId
      (SetChatId chat, WithUserName user) -> do
        _ <- fork $ io $ do
          alterStore store (ChattingMsg Sync)
        StateChatting <$> toChatting user chat
      (ChattingMsg m, StateChatting s) -> io $ StateChatting <$> transform m s
      (_, _) -> do
        io $ putStrLn ("state/msg mismatch, discarding message: " ++ show msg)
        return state

instance StoreData Chatting where
  type StoreAction Chatting = ChattingMsg
  transform = \ case
    Sync -> \ state@(Chatting _ chatId doc _) -> do
      _ <- forkIO $ do
        putStrLn "syncing..."
        baseUrl <- sameOriginBaseUrl Nothing
        result <- runExceptT $
          sync chatId doc (error "manager shouldn't be touched") baseUrl
        putStrLn "received sync data..."
        case result of
          Right new ->
            alterStore store $ ChattingMsg $ Update new
          Left err ->
            alterStore store $ Error $ cs $ show err

        threadDelay 10000000
        alterStore store (ChattingMsg Sync)

      return state

    Update new -> \ (Chatting cid chatId doc index) -> do
      let cursor = toCursor doc index
          newDoc = doc <> new
      return $ Chatting cid chatId newDoc (fromCursor newDoc cursor)

    Enter newMessage -> \ (Chatting cid chatId oldDoc cursor) -> do
      let oldVector = getVector oldDoc
          newVector = insertAt cursor newMessage oldVector
          newDoc = oldDoc <> mkPatch cid oldDoc newVector
      return $ Chatting cid chatId newDoc (succ cursor)
    UpArrow -> \ (Chatting cid chatId doc cursor) ->
      return $ Chatting cid chatId doc (max 0 (cursor - 1))
    DownArrow -> \ (Chatting cid chatId doc cursor) ->
      return $ Chatting cid chatId doc (min (length $ getVector doc) (cursor + 1))

-- * transform utils

insertAt :: Int -> a -> [a] -> [a]
insertAt i e list =
  let (prefix, suffix) = splitAt i list in
  prefix ++ [e] ++ suffix

-- * view

viewPatches :: ReactView ()
viewPatches = defineControllerView "patches app" store $ \ model () -> do
  viewErrors $ errors model
  case state model of
    Empty -> center $ do
      view (focusedInput "name:" SetUserName) () mempty
    WithUserName _ -> center $ "loading..."
    StateChatting chatting -> center $ do
      viewChatMessages chatting
      viewDebug chatting

viewErrors :: [Text] -> ReactElementM ViewEventHandler ()
viewErrors = \ case
  [] -> return ()
  errs -> do
    "Some errors were encountered:"
    ul_ [] $ do
      forM_ errs $ \ err -> do
        li_ [] $ do
          elemText err
    hr_ []

viewChatMessages :: Chatting -> ReactElementM ViewEventHandler ()
viewChatMessages state = do
  div_ [arrowEvents] $ do
    let elements = zip (Nothing : map Just (getVectorWithClients (document state))) [0 ..]
    forM_ elements $ \ (message, index) -> do
      forM_ message $ \ (Client (_, userName), m) -> do
        elemText (userName <> ": " <> m)
        br_ []
      when (index == cursor state) $ do
        view (focusedInput ">>>" (ChattingMsg . Enter)) () mempty

viewDebug :: Chatting -> ReactElementM ViewEventHandler ()
viewDebug state = do
  hr_ []
  h4_ [] "debugging:"
  pre_ $ fromString $ ppTree $ document state
  br_ []
  text_ $ fromString $ show (getVector $ document state)
  br_ []
  text_ $ fromString $ show state
  br_ []

-- * view utils

focusedInput :: Text -> (Text -> Msg) -> ReactView ()
focusedInput label f = defineStatefulView "chat input" "" $ \ (text :: Text) () -> do
  elemText (label <> " ")
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
  maybe [] (\ msg -> [SomeStoreAction store (ChattingMsg msg)]) $
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
