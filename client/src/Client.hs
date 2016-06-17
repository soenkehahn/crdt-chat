{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Client where

import           Control.Concurrent
import           Control.DeepSeq
import           Control.Monad.Trans.Except
import           Data.Crdt.TreeVector
import           Data.Crdt.TreeVector.Internal
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
instance NFData Data.Crdt.TreeVector.Client

instance StoreData Model where
  type StoreAction Model = Msg
  transform = \ case
    Sync -> \ model@(Model doc _) -> do
      _ <- forkIO $ do
        baseUrl <- sameOriginBaseUrl Nothing
        result <- runExceptT $
          sync doc (error "manager shouldn't be touched") baseUrl
        _ <- forkIO (threadDelay 1000000 >> alterStore store Sync)
        case result of
          Right new ->
            alterStore store $ Update new
      return model

    Update new -> \ (Model doc inputField) -> do
      return $ Model (doc <> new) inputField

    Input s -> \ (Model doc _) ->
      return $ Model doc s
    Enter -> \ (Model old new) ->
      return $ Model (old <> mkPatch (Client 0) old (getVector old ++ [new])) ""

    Debug msg -> \ model -> putStrLn msg $> model

viewPatches :: ReactView ()
viewPatches = defineControllerView "patches app" store $ \ model () -> do
  ul_ [] $ do
    forM_ (getVector (document model)) $ \ message -> do
      li_ [] $ do
        text_ $ fromString $ cs message
  view chatInput () mempty
  br_ []
  text_ $ fromString $ show (getVector $ document model)
  br_ []
  text_ $ fromString $ show model

chatInput :: ReactView ()
chatInput = defineControllerView "chat input" store $ \ model () -> do
  input_ $
    ("value" &= inputField model) :
    (onInput $ \ event ->
       SomeStoreAction store (Input (target event "value")) :
       SomeStoreAction store (Debug (show event)) :
       []) :
    (onEnter $ \ _ _ -> [SomeStoreAction store Enter]) :
    []

-- * view utils

onEnter :: (Event -> KeyboardEvent -> [SomeStoreAction]) -> PropertyOrHandler [SomeStoreAction]
onEnter action = onKeyDown $ \ event keyboardEvent ->
  case keyCode keyboardEvent of
    13 -> action event keyboardEvent
    _ -> []

textfield_ :: [PropertyOrHandler eventHandler] -> ReactElementM eventHandler ()
textfield_ props = textarea_ props (return ())
