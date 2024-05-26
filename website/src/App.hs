module App where

import Spam

appName :: String
appName = "Website"

app :: Kitchen
app =
  Kitchen
    { name = appName,
      env = Dev
    }