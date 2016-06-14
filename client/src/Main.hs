{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

import           Data.String
import           React.Flux

import           Api

main :: IO ()
main = reactRender "main" viewPatches ()

store :: ReactStore Document
store = mkStore mempty

instance StoreData Document where
  type StoreAction Document = ()
  transform () doc = return doc

viewPatches :: ReactView ()
viewPatches = defineControllerView "patches app" store $ \ doc () ->
  text_ (fromString (show doc))
