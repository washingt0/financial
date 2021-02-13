{-# LANGUAGE OverloadedStrings #-}

module Config.Types
  ( DatabaseConn
  , Config
  , defaultConfig
  , db
  , password
  , defaultDBConn
  , parseConfig
  ) where

import Data.Aeson (FromJSON(parseJSON), Value(Object), (.:?), withObject, eitherDecode)
import qualified Data.ByteString.Lazy as B
import Data.Text 
import Data.Maybe

data DatabaseConnJ =
  DatabaseConnJ
    { _host :: Maybe Text
    , _username :: Maybe Text
    , _password :: Maybe Text
    }
  deriving (Show)

instance FromJSON DatabaseConnJ where
  parseJSON (Object v) =
    DatabaseConnJ <$> v .:? "host" <*> v .:? "username" <*> v .:? "password"


data ConfigJ =
  ConfigJ
    { _db :: Maybe DatabaseConnJ
    , _logLevel :: Maybe Int
    }
  deriving (Show)

instance FromJSON ConfigJ where
  parseJSON (Object v) = ConfigJ <$> v .:? "db" <*> v .:? "log_level"

data DatabaseConn =
  DatabaseConn
    { host :: String
    , username :: String
    , password :: String
    }
  deriving (Show)

defaultDBConn =
  DatabaseConn {host = "localhost", username = "ff", password = "123456"}

data Config =
  Config
    { db :: DatabaseConn
    , logLevel :: Int
    }
  deriving (Show)

defaultConfig = Config {db = defaultDBConn, logLevel = 0}

mergeDBConfig :: DatabaseConnJ -> DatabaseConn
mergeDBConfig cfgJSON = do
    let std = defaultDBConn
    DatabaseConn {
        host = unpack $ fromMaybe (pack $ host std) $  _host cfgJSON 
        , username = unpack $ fromMaybe (pack $ username std) $ _username cfgJSON
        , password = unpack $ fromMaybe (pack $ password std) $ _password cfgJSON
    }


mergeConfig :: ConfigJ -> Config
mergeConfig cfgJSON = do
    let std = defaultConfig
    Config {
        db = maybe defaultDBConn mergeDBConfig $ _db cfgJSON
          , logLevel = fromMaybe  (logLevel std) $ _logLevel cfgJSON
    }

parseConfig :: IO B.ByteString -> IO Config
parseConfig xs = do
    c <- (eitherDecode <$> xs) :: IO (Either String ConfigJ)
    case c of
        Left _ -> return defaultConfig
        Right c -> return $ mergeConfig c