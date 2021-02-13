{-# LANGUAGE OverloadedStrings #-}

module Main where

import Config.Config (getDBConfig, loadConfig)
import Control.Applicative
import Lib (getConnection, getCurrentDate, performSelect)
import Snap.Core
import Snap.Http.Server

import Interface.Routes as R

main :: IO ()
main = do
  cfg <- loadConfig
  let conn = getConnection (getDBConfig cfg)
  putStrLn =<< getCurrentDate conn
  quickHttpServe site

site :: Snap ()
site = ifTop (writeBS ":)") <|> R.routingTree
