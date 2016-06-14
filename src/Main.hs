
import           Network.Wai
import           Network.Wai.Handler.Warp as Warp
import App

main :: IO ()
main = do
  let port = 8000
      settings =
        setPort port $
        setBeforeMainLoop (putStrLn ("listening on port " ++ show port)) $
        defaultSettings
  Warp.runSettings settings =<< mkApp
