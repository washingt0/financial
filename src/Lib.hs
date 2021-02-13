{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( someFunc
  , performSelect
  , getConnection
  , getCurrentDate
  ) where

import Config.Types
import Data.Maybe
import qualified Data.Text as Text
import Database.PostgreSQL.Simple
  ( ConnectInfo(connectDatabase, connectPassword)
  , Connection
  , Only(Only)
  , connect
  , defaultConnectInfo
  , query
  , query_
  )

someFunc :: IO ()
someFunc = putStrLn "someFunc"

getConnection :: DatabaseConn -> IO Connection
getConnection connCfg =
  connect
    defaultConnectInfo
      {connectDatabase = "postgres", connectPassword = password connCfg}

performSelect :: IO Connection -> Int -> IO ()
performSelect conn x = do
  cc <- conn
  mapM_ print =<< (query cc "SELECT ? * 2" (Only x) :: IO [Only Int])

getCurrentDate :: IO Connection -> IO [Char]
getCurrentDate conn = do
  cc <- conn
  [Only i] <- query_ cc "SELECT NOW()::TEXT"
  return i
