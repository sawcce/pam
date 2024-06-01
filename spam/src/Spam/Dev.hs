{-# LANGUAGE OverloadedStrings #-}

module Spam.Dev where

import Control.Concurrent (MVar, forkIO, newEmptyMVar, putMVar, swapMVar, tryPutMVar, tryReadMVar)
import Control.Concurrent.Async (async)
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.Foldable qualified
import Data.List (find, isSuffixOf)
import GHC.Conc (threadDelay)
import Language.Haskell.Interpreter (OptionVal (..), as, interpret, loadModules, searchPath, set, setImports, setTopLevelModules)
import Language.Haskell.Interpreter.Unsafe (unsafeRunInterpreterWithArgs)
import Network.HTTP.Types (status404, status500)
import Network.HTTP.Types.Status (status200)
import Network.Wai (Application, Request (..), Response, ResponseReceived, responseFile, responseLBS)
import Network.Wai.Handler.Warp (run)
import Spam (File (filePath, fileURL), Kitchen (..), Route (..))
import Spam.El (render)
import Spam.Log qualified as Log
import System.FSNotify (watchDir, watchTree, withManager)

data DevConfig = DevConfig
  { packageDB :: String,
    appPath :: String,
    searchDirs :: [String]
  }

watch :: DevConfig -> IO ()
watch devConfig = withManager $ \mgr -> do
  appVar :: MVar Kitchen <- newEmptyMVar
  result <- load devConfig
  Data.Foldable.forM_ result (putMVar appVar)
  forkIO $ run 3000 (application appVar)

  _ <- async $ watchDir mgr "src/" (const True) $ \_event -> do
    Log.info $ show _event
    Log.info "Reloading server..."
    result <- load devConfig
    Data.Foldable.forM_ result (swapMVar appVar)
    Log.info "Server reloaded!"
  forever $ threadDelay 100000

application :: MVar Kitchen -> Application
application app request respond = do
  maybeKitchen <- tryReadMVar app
  case maybeKitchen of
    Nothing -> respond $ responseLBS status500 [("Content-Type", "text/plain")] "No app active!"
    Just kitchen -> do
      respond $ matchRequest kitchen request

matchRequest :: Kitchen -> Request -> Response
matchRequest (Kitchen _name routes files) request = do
  let requestPath = rawPathInfo request
      route = find (\x -> BS.pack (path x) == requestPath) routes
      file = find (\x -> BS.pack (fileURL x) == requestPath) files
  case (route, file) of
    (Just r, _) -> responseLBS status200 [("Content-Type", "text/html")] (BSL.pack $ render (contents r))
    (_, Just f) -> responseFile status200 [("Content-Type", getContentType $ filePath f)] (filePath f) Nothing
    _ -> responseLBS status404 [] "404 :/"

getContentType :: FilePath -> BS.ByteString
getContentType path
  | ".html" `isSuffixOf` path = "text/html"
  | ".css" `isSuffixOf` path = "text/css"
  | ".js" `isSuffixOf` path = "application/javascript"
  | ".png" `isSuffixOf` path = "image/png"
  | ".jpg" `isSuffixOf` path = "image/jpeg"
  | otherwise = "application/octet-stream"

load :: DevConfig -> IO (Maybe Kitchen)
load (DevConfig packageDB appPath searchDirs) = do
  result <- unsafeRunInterpreterWithArgs ["-package-db " <> packageDB] $ do
    liftIO $ Log.info ("Loading " <> appPath)
    set [searchPath := searchDirs]
    liftIO $ Log.info "Search paths set"
    setImports ["Prelude"]
    liftIO $ Log.info "Imports set"

    loadModules [appPath]
    liftIO $ Log.info ("Loaded " <> appPath)
    setTopLevelModules ["App", "Spam", "Spam.El"]
    liftIO $ Log.info ("Interpreting " <> appPath)
    interpret "app" (as :: Kitchen)
  liftIO $ Log.info "App loaded!"

  case result of
    Left err -> do
      putStrLn $ "Error: " ++ show err
      return Nothing
    Right app -> do return (Just app)