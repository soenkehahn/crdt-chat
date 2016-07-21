
import           Network.Wai
import           Network.Wai.Handler.Warp as Warp

import           Network.Wai.Application.CrdtChat

main :: IO ()
main = do
  let port = 8000
      settings =
        setPort port $
        setBeforeMainLoop (putStrLn ("listening on port " ++ show port)) $
        defaultSettings
  Warp.runSettings settings =<< mkApp
