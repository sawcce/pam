module Main where

import Configuration.Dotenv
import Spam.Dev
import System.Environment

main :: IO ()
main = do
  loadFile defaultConfig
  packageDBPath <- getEnv "PACKAGE_DB"

  watch
    DevConfig
      { appPath = "src/App.hs",
        searchDirs = ["src", "../spam/src"],
        packageDB = packageDBPath
      }
