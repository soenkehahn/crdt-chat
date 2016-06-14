{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Client where

import           Data.String
import           React.Flux

import           Api

run :: IO ()
run = reactRender "main" viewPatches ()

store :: ReactStore Document
store = mkStore mempty

instance StoreData Document where
  type StoreAction Document = ()
  transform () doc = return doc

viewPatches :: ReactView ()
viewPatches = defineControllerView "patches app" store $ \ doc () -> do
  text_ (fromString (show doc))
  text_ "bla"
