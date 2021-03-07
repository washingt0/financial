{-# LANGUAGE DeriveGeneric #-}

module Middleware.UserInfo
  (
  ) where

import Data.Aeson
import GHC.Generics

data AuthenticatedUser = AUser { 
  name :: String
  , userName :: String
  , id :: String
} deriving (Show, Generic)

instance FromJSON AuthenticatedUser