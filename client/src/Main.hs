{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

import           React.Flux

main :: IO ()
main = reactRender "main" viewPatches ()

store :: ReactStore ()
store = mkStore ()

instance StoreData () where
  type StoreAction () = ()
  transform () () = return ()

viewPatches :: ReactView ()
viewPatches = defineControllerView "patches app" store $ \ () () ->
  text_ "huhu"
