{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Api where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Crdt.TreeVector
import           Data.Crdt.TreeVector.Internal
import           Data.Map
import           Data.Proxy
import           Data.Text
import           Servant.API
import           Text.Read

type Api =
  "api" :> "sync" :> ReqBody '[JSON] Document :> Post '[JSON] Document :<|>
  Raw

api :: Proxy Api
api = Proxy

-- * types

type Document = TreeVector Text

instance FromJSON a => FromJSON (TreeVector a)
instance FromJSON a => FromJSON (Map Client a) where
  parseJSON v = do
    m :: Map String a <- parseJSON v
    fromList <$> mapM (firstM convert) (toList m)
    where
      convert :: String -> Parser Client
      convert s = case readMaybe s of
        Just id -> return $ Client id
        Nothing -> fail ("not a client id: " ++ s)
instance FromJSON a => FromJSON (Node a)
instance FromJSON a => FromJSON (Element a)

instance ToJSON a => ToJSON (TreeVector a)
instance ToJSON a => ToJSON (Map Client a) where
  toJSON m = toJSON (mapKeys convert m :: Map String a)
    where
      convert (Client id) = show id
instance ToJSON a => ToJSON (Node a)
instance ToJSON a => ToJSON (Element a)

firstM :: Functor m => (a -> m b) -> (a, c) -> m (b, c)
firstM f (a, c) = (, c) <$> f a
