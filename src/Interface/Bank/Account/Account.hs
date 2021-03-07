{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module Interface.Bank.Account.Account
  ( AccountAPI
  , accountServer
  ) where

import Data.Pool
import Database.PostgreSQL.Simple
import Servant
import Servant.API
import Servant.API.ContentTypes

import Data.Aeson
import Data.Aeson.Types
import GHC.Generics

type AccountAPI
   = Get '[ JSON] String :<|> Capture "id" Int :> Get '[ JSON] String

accountServer :: Pool Connection -> Server AccountAPI
accountServer p = getAll p :<|> getOne p

getAll :: Pool Connection -> Handler String
getAll = error "getAll"

getOne :: Pool Connection -> Int -> Handler String
getOne _ = error "getOne"
