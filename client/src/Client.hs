{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Client where

import           Data.Monoid
import           Data.String
import           React.Flux

import           Api

run :: IO ()
run = reactRender "main" viewPatches ()

data Model
  = Model {
    document :: Document
  }
  deriving (Eq, Show)

store :: ReactStore Model
store = mkStore (Model mempty)

data Msg
  = Update Document

instance StoreData Model where
  type StoreAction Model = Msg
  transform msg (Model doc) = case msg of
    Update new -> return $ Model (doc <> new)

viewPatches :: ReactView ()
viewPatches = defineControllerView "patches app" store $ \ doc () -> do
  text_ (fromString (show (document doc)))
  text_ "bla"
