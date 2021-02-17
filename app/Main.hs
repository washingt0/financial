{-# LANGUAGE OverloadedStrings #-}

module Main where

import Config.Config (getDBConfig, loadConfig)
import Config.Types (listenPort)
import Control.Applicative ( Alternative((<|>)) )
import Lib (getConnection, getCurrentDate, performSelect)

import Interface.Routes as R

import Servant.API
import Servant

import Network.Wai.Handler.Warp

app :: Application
app = serve baseAPI baseServer

main :: IO ()
main = do
  cfg <- loadConfig
  run (listenPort cfg) app
