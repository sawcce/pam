{-# LANGUAGE OverloadedStrings #-}

module Spam.Log where

log' :: String -> String -> IO ()
log' p m = putStrLn $ "[" <> p <> "] " <> m

info :: String -> IO ()
info = log' "INFO"