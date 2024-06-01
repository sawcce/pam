module App where

import Routes
import Spam

appName :: String
appName = "Website"

app :: Kitchen
app =
  Kitchen
    { name = appName,
      routes = appRoutes,
      files =
        [ File
            { filePath = "./public/index.css",
              fileURL = "/index.css"
            }
        ]
    }
