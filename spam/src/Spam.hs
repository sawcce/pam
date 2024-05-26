module Spam where

data Target = Prod | Dev deriving (Show)

data Kitchen where
  Kitchen ::
    { name :: String,
      env :: Target
    } ->
    Kitchen
  deriving (Show)