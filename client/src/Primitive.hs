{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Primitive (
  module Primitive.Types,
  run,
) where

#ifdef __GHCJS__

import           Control.Concurrent
import           Control.Monad.Operational
import           Control.Monad.Trans.Except
import           Data.JSString
import           Data.UUID as UUID
import           Network.HTTP.Client
import           React.Flux (alterStore)
import           Servant.API
import           Servant.Client
import           System.Random

import           Api
import           SameOrigin

#endif

import           Primitive.Types

run :: ClientCommand a -> IO a


#ifndef __GHCJS__

run = error "Primitive.run on ghc is not supported."

#else

new :: Manager -> BaseUrl -> ClientM ChatId
new :<|> _ = client api

run command = case view command of
  Return a -> return a
  prim :>>= next -> (>>= run . next) $ case prim of
    IO io -> io
    Fork action -> do
      _ <- forkIO $
        run action
      return ()
    NewUuid -> randomIO
    GetHashFragment -> do
      hash <- js_getHash
      print hash
      case unpack hash of
        "" -> return $ Right Nothing
        '#' : (UUID.fromString -> Just uuid) ->
          return $ Right $ Just $ ChatId uuid
        _ -> return $ Left $
          "invalid uuid: " ++ unpack hash
    SetHashFragment (ChatId uuid) -> do
      js_setHash $ pack $ UUID.toString uuid
    NewChat -> do
      baseUrl <- sameOriginBaseUrl Nothing
      runExceptT $ new (error "manager") baseUrl
    AlterStore store msg -> do
      _ <- forkIO $ alterStore store msg
      return ()

foreign import javascript unsafe "document.location.hash"
  js_getHash :: IO JSString

foreign import javascript unsafe "document.location.hash = $1"
  js_setHash :: JSString -> IO ()

#endif
