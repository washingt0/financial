{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Config.Config
import Config.Types
import Control.Applicative (Alternative((<|>)))

import Interface.Routes as R

import Servant
import Servant.API

import Network.Wai.Handler.Warp

import Database.Database

import Data.Pool
import Database.PostgreSQL.Simple

import Control.Monad.Trans.Control

app :: Application
app =  serve baseAPI baseServer


main :: IO ()
main = do
  cfg <- loadConfig
  checkMigration (getConnection (db cfg)) "0000"
  run (listenPort cfg) app 
