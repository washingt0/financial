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

import Database.Database (checkMigration, getConnection, initializePool)

import Data.Pool
import Database.PostgreSQL.Simple

import Control.Monad.Trans.Control

app :: Pool Connection -> Application
app pool = serve baseAPI $ baseServer pool

main :: IO ()
main = do
  cfg <- loadConfig
  checkMigration (getConnection (db cfg)) "0000"
  pool <- initializePool (db cfg)
  run (listenPort cfg) $ app pool
