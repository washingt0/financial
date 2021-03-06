{-# LANGUAGE OverloadedStrings #-}

module Database.Database
  ( checkMigration
  , getConnection
  , initializePool
  ) where

import Config.Types

import Database.PostgreSQL.Simple
  ( ConnectInfo(connectDatabase, connectHost, connectPassword,
            connectUser)
  , Connection
  , Only(Only)
  , close
  , connect
  , defaultConnectInfo
  , query
  )

import Control.Exception

import Data.Typeable

import Data.Pool

data MigrationException =
  InvalidMigrationVersion
  deriving (Show, Typeable)

instance Exception MigrationException

getConnection :: DatabaseConn -> IO Connection
getConnection connCfg =
  connect
    defaultConnectInfo
      { connectDatabase = database connCfg
      , connectPassword = password connCfg
      , connectHost = host connCfg
      , connectUser = username connCfg
      }

initializePool :: DatabaseConn -> IO (Pool Connection)
initializePool connCfg = createPool (getConnection connCfg) close 2 60 10

checkMigration :: IO Connection -> String -> IO Bool
checkMigration conn minMigration = do
  cc <- conn
  [Only i] <-
    query
      cc
      "\
		\ SELECT name >= ?::TEXT\
		\ FROM public.t_migration\
		\ WHERE rolled_back = FALSE\
			\ AND deleted_at IS NULL\
		\ ORDER BY name DESC\
		\ LIMIT 1\
	\"
      (Only minMigration)
  if i
    then return i
    else throw InvalidMigrationVersion
