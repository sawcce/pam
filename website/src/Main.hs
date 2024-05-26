module Main where

import App (appName)
import Spam

app :: Kitchen
app =
  Kitchen
    { name = appName,
      env = Prod
    }

main :: IO ()
main = print app
