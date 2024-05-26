module Main where

import App (appName, appRoutes)
import Spam

app :: Kitchen
app =
  Kitchen
    { name = appName,
      routes = appRoutes
    }

main :: IO ()
main = serve Prod app
