module Spam where

import Network.HTTP.Types (status200)
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Spam.El
import Spam.El.Html

data Target = Prod | Dev deriving (Show)

data Kitchen where
  Kitchen ::
    { name :: String,
      routes :: [Route]
    } ->
    Kitchen
  deriving (Show)

data Route where
  Route ::
    { path :: String,
      contents :: El
    } ->
    Route
  deriving (Show)

serve :: Target -> Kitchen -> IO ()
serve Dev (Kitchen name routes) = print "Not implemented yet"
serve Prod (Kitchen name routes) = run 3000 application

application _ respond = respond $
  responseLBS status200 [("Content-Type", "text/plain")] "Hello World"


-- mkRoute (Route path el) = get path $ html (render el)