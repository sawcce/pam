module App where

import Spam
import Spam.El
import Spam.El.Html

appName :: String
appName = "Website"

app :: Kitchen
app =
  Kitchen
    { name = appName,
      routes = appRoutes
    }

appRoutes :: [Route]
appRoutes = [helloRoute]

helloRoute :: Route
helloRoute =
  Route
    { path = "/",
      contents = html' [] $ body' [] $ p' [] (String "Hey there!")
    }
