module Config.Config
(
    loadConfig
    , getDBConfig
) where

import System.Environment
import Data.Maybe
import Config.Types


import qualified Data.ByteString.Lazy as B

loadConfig :: IO Config
loadConfig = do
    x <- lookupEnv "FF_CONFIG"
    parseConfig (B.readFile (fromMaybe "config.json"  x :: FilePath))
    
getDBConfig :: Config -> DatabaseConn
getDBConfig = db