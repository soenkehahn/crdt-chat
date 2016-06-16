{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Api where

import           CRDT.TreeVector
import           CRDT.TreeVector.Internal
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Map
import           Data.Proxy
import           Servant.API
import           Text.Read

type Api =
  "api" :> "sync" :> ReqBody '[JSON] Document :> Post '[JSON] Document :<|>
  Raw

api :: Proxy Api
api = Proxy

-- * types

type Document = TreeVector Char

instance FromJSON Document
instance FromJSON a => FromJSON (Map Client a) where
  parseJSON v = do
    m :: Map String a <- parseJSON v
    fromList <$> mapM (firstM convert) (toList m)
    where
      convert :: String -> Parser Client
      convert s = case readMaybe s of
        Just id -> return $ Client id
        Nothing -> fail ("not a client id: " ++ s)
instance FromJSON (Node Char)
instance FromJSON (Element Char)

instance ToJSON Document
instance ToJSON a => ToJSON (Map Client a) where
  toJSON m = toJSON (mapKeys convert m :: Map String a)
    where
      convert (Client id) = show id
instance ToJSON (Node Char)
instance ToJSON (Element Char)

firstM :: Functor m => (a -> m b) -> (a, c) -> m (b, c)
firstM f (a, c) = (, c) <$> f a
