{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module SameOrigin where

#ifdef __GHCJS__

import           GHCJS.Prim (fromJSString, fromJSInt)
import           JavaScript.Object
import           Servant.Client
import           Text.Read

sameOriginBaseUrl :: IO (String -> BaseUrl)
sameOriginBaseUrl = do
  location <- js_location
  js_protocol <- fromJSString <$> getProp "protocol" location
  let protocol = case js_protocol of
        "http:" -> Http
        "https:" -> Https
        _ -> error ("unparseable protocol: " ++ js_protocol)
  host <- fromJSString <$> getProp "hostname" location
  js_port <- fromJSString <$> getProp "port" location
  let port = case js_port of
        "" -> case protocol of
          Http -> 80
          Https -> 443
        _ -> case readMaybe js_port of
          Just p -> p
          Nothing -> error ("unparseable port: " ++ js_port)
  return $ BaseUrl protocol host port

foreign import javascript unsafe "(function () { return location; })()"
  js_location :: IO Object

#else

import           Servant.Client

sameOriginBaseUrl :: IO (String -> BaseUrl)
sameOriginBaseUrl = do
  error "sameOriginBaseUrl through GHC"

#endif
