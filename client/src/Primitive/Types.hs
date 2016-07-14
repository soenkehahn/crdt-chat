{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Primitive.Types where

import           Control.Monad.Operational
import           Data.Typeable
import           Data.UUID
import           React.Flux hiding (view)
import           Servant.Client

import           Api

type ClientCommand = Program Primitive

data Primitive a where
  IO :: Typeable a => IO a -> Primitive a
  Fork :: ClientCommand () -> Primitive ()

  GetHashFragment :: Primitive (Either String (Maybe ChatId))
  SetHashFragment :: ChatId -> Primitive ()
  NewUuid :: Primitive UUID
  NewChat :: Primitive (Either ServantError ChatId)

  AlterStore :: (StoreData storeData, Typeable (StoreAction storeData)) =>
    ReactStore storeData -> StoreAction storeData -> Primitive ()

io :: Typeable a => IO a -> ClientCommand a
io = singleton . IO

fork :: ClientCommand () -> ClientCommand ()
fork action = singleton $ Fork action

getHashFragment :: ClientCommand (Either String (Maybe ChatId))
getHashFragment = singleton GetHashFragment

setHashFragment :: ChatId -> ClientCommand ()
setHashFragment = singleton . SetHashFragment

newUuid :: ClientCommand UUID
newUuid = singleton NewUuid

newChat :: ClientCommand (Either ServantError ChatId)
newChat = singleton NewChat

alterStoreP :: (StoreData storeData, Typeable (StoreAction storeData)) =>
    ReactStore storeData -> StoreAction storeData -> ClientCommand ()
alterStoreP store msg = singleton $ AlterStore store msg

