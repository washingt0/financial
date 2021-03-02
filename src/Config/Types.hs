{-# LANGUAGE OverloadedStrings #-}

module Config.Types
  ( DatabaseConn
  , Config
  , defaultConfig
  , db
  , host
  , database
  , username
  , password
  , defaultDBConn
  , parseConfig
  , listenPort
  ) where

import Data.Aeson
  ( FromJSON(parseJSON)
  , Value(Object)
  , (.:?)
  , eitherDecode
  , withObject
  )
import qualified Data.ByteString.Lazy as B
import Data.Maybe
import Data.Text

data DatabaseConnJ =
  DatabaseConnJ
    { _host :: Maybe Text
    , _username :: Maybe Text
    , _password :: Maybe Text
    , _database :: Maybe Text
    }
  deriving (Show)

instance FromJSON DatabaseConnJ where
  parseJSON (Object v) =
    DatabaseConnJ <$> v .:? "host" <*> v .:? "username" <*> v .:? "password" <*>
    v .:? "database"

data ConfigJ =
  ConfigJ
    { _db :: Maybe DatabaseConnJ
    , _logLevel :: Maybe Int
    , _listenPort :: Maybe Int
    }
  deriving (Show)

instance FromJSON ConfigJ where
  parseJSON (Object v) =
    ConfigJ <$> v .:? "db" <*> v .:? "log_level" <*> v .:? "listen_port"

data DatabaseConn =
  DatabaseConn
    { host :: String
    , username :: String
    , password :: String
    , database :: String
    }
  deriving (Show)

defaultDBConn =
  DatabaseConn
    { host = "localhost"
    , username = "financial"
    , password = "123456"
    , database = "financial"
    }

data Config =
  Config
    { db :: DatabaseConn
    , logLevel :: Int
    , listenPort :: Int
    }
  deriving (Show)

defaultConfig = Config {db = defaultDBConn, logLevel = 0, listenPort = 7000}

mergeDBConfig :: DatabaseConnJ -> DatabaseConn
mergeDBConfig cfgJSON = do
  let std = defaultDBConn
  DatabaseConn
    { host = unpack $ fromMaybe (pack $ host std) $ _host cfgJSON
    , username = unpack $ fromMaybe (pack $ username std) $ _username cfgJSON
    , password = unpack $ fromMaybe (pack $ password std) $ _password cfgJSON
    , database = unpack $ fromMaybe (pack $ database std) $ _database cfgJSON
    }

mergeConfig :: ConfigJ -> Config
mergeConfig cfgJSON = do
  let std = defaultConfig
  Config
    { db = maybe defaultDBConn mergeDBConfig $ _db cfgJSON
    , logLevel = fromMaybe (logLevel std) $ _logLevel cfgJSON
    , listenPort = fromMaybe (listenPort std) $ _listenPort cfgJSON
    }

parseConfig :: IO B.ByteString -> IO Config
parseConfig xs = do
  c <- (eitherDecode <$> xs) :: IO (Either String ConfigJ)
  case c of
    Left _ -> return defaultConfig
    Right c -> return $ mergeConfig c
