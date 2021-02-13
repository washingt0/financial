module Config.Config
  ( loadConfig
  , getDBConfig
  ) where

import Config.Types
import Data.Maybe
import System.Environment

import qualified Data.ByteString.Lazy as B

loadConfig :: IO Config
loadConfig = do
  x <- lookupEnv "FF_CONFIG"
  parseConfig (B.readFile (fromMaybe "config.json" x :: FilePath))

getDBConfig :: Config -> DatabaseConn
getDBConfig = db
