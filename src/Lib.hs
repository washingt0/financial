{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( someFunc
  , performSelect
  , getConnection
  , getCurrentDate
  ) where

import Database.PostgreSQL.Simple
    ( query,
      query_,
      connect,
      defaultConnectInfo,
      Only(Only),
      ConnectInfo(connectDatabase, connectPassword),
      Connection )
import qualified Data.Text as Text
import Config.Types
import Data.Maybe

someFunc :: IO ()
someFunc = putStrLn "someFunc"

getConnection :: DatabaseConn -> IO Connection
getConnection connCfg =
  connect
    defaultConnectInfo
      {connectDatabase = "postgres", connectPassword = password connCfg }

performSelect :: IO Connection -> Int -> IO ()
performSelect conn x = do
  cc <- conn
  mapM_ print =<< (query cc "SELECT ? * 2" (Only x) :: IO [Only Int])

getCurrentDate :: IO Connection -> IO [Char]
getCurrentDate conn = do
  cc <- conn
  [Only i] <- query_ cc "SELECT NOW()::TEXT"
  return i