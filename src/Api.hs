{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Api where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Crdt.TreeVector as Crdt
import           Data.Crdt.TreeVector.Internal
import           Data.Map as Map
import           Data.Proxy
import           Data.String.Conversions
import           Data.Text (Text)
import           Data.UUID
import           GHC.Generics
import           Prelude hiding (lookup)
import           Servant.API

type Api =
  "api" :> "new" :> Post '[JSON] ChatId :<|>
  "api" :> Capture "chat-id" ChatId :> "sync" :> ReqBody '[JSON] Document
    :> Post '[JSON] Document :<|>
  Capture "chat-id" ChatId :> Raw :<|>
  Raw

api :: Proxy Api
api = Proxy

-- * types

instance FromJSON UUID where
  parseJSON v = case v of
    String s -> case fromString $ cs s of
      Just uuid -> return uuid

data ChatId = ChatId UUID
  deriving (Eq, Ord, Show, Generic)

nilChatId :: ChatId
nilChatId = ChatId nil

instance ToHttpApiData ChatId where
  toUrlPiece (ChatId uuid) = toText uuid

instance FromHttpApiData ChatId where
  parseUrlPiece s =
    maybe (Left ("not a valid uuid: " <> s)) (Right . ChatId) $
      fromString $ cs s

instance ToJSON ChatId where
  toJSON (ChatId uuid) = object $
    "chatId" .= toString uuid :
    []

instance FromJSON ChatId where
  parseJSON = withObject "UUID" $ \ o -> do
    ChatId <$> (o .: "chatId")

type Document = TreeVector CId Text

type CId = (UUID, Text)

nilClient :: Crdt.Client CId
nilClient = Client (nil, "anonymous")

instance FromJSON a => FromJSON (TreeVector CId a)
instance FromJSON a => FromJSON (Map (Client CId)  a) where
  parseJSON v = do
    m :: Map String a <- parseJSON v
    Map.fromList <$> mapM (firstM convert) (Map.toList m)
    where
      convert :: String -> Parser (Client CId)
      convert (words -> [fromString -> Just uuid, userName]) =
        return $ Client (uuid, cs userName)
      convert s =
        fail ("not a client id: " ++ s)
instance FromJSON a => FromJSON (Node CId a)
instance FromJSON a => FromJSON (Element a)

instance ToJSON a => ToJSON (TreeVector CId a)
instance ToJSON a => ToJSON (Map (Client CId) a) where
  toJSON m = toJSON (mapKeys convert m :: Map String a)
    where
      convert :: Client (UUID, Text) -> String
      convert (Client (uuid, userName)) = toString uuid ++ " " ++ cs userName
instance ToJSON a => ToJSON (Node CId a)
instance ToJSON a => ToJSON (Element a)

firstM :: Functor m => (a -> m b) -> (a, c) -> m (b, c)
firstM f (a, c) = (, c) <$> f a
