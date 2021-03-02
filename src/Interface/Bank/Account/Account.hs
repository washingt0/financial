{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module Interface.Bank.Account.Account
  ( AccountAPI
  , accountServer
  ) where

import Servant
import Servant.API
import  Servant.API.ContentTypes
import Data.Pool
import Database.PostgreSQL.Simple

import Data.Aeson
import Data.Aeson.Types
import GHC.Generics

newtype HelloMessage = HelloMessage { msg :: String }
  deriving Generic

instance ToJSON HelloMessage

type AccountAPI
   = Get '[ JSON] String :<|> Capture "id" Int :> Get '[ JSON] HelloMessage

accountServer :: Server AccountAPI
accountServer = getAll :<|> getOne

getAll :: Handler String
getAll = error "getAll"

getOne :: Int -> Handler HelloMessage
getOne _ = return (HelloMessage "AAA")
