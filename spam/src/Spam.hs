{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Spam where

import Spam.El

data Target = Prod | Dev deriving (Show)

data Kitchen where
  Kitchen ::
    { name :: String,
      routes :: [Route],
      files :: [File]
    } ->
    Kitchen
  deriving (Show)

data Layout = Layout String (El -> El)

instance Show Layout where
  show (Layout name _) = "(Layout " <> name <> ")"

data Route where
  Route ::
    { path :: String,
      contents :: El,
      routeLayouts :: [Layout]
    } ->
    Route
  deriving (Show)

data File where
  File ::
    { filePath :: String,
      fileURL :: String
    } ->
    File
  deriving (Show)
