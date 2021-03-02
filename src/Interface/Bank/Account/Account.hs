{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Interface.Bank.Account.Account
  ( AccountAPI
  , accountServer
  ) where

import Servant
import Servant.API
import Data.Pool
import Database.PostgreSQL.Simple

type AccountAPI
   = Get '[ JSON] String :<|> Capture "id" Int :> Get '[ JSON] String

accountServer :: Connection -> Server AccountAPI
accountServer conn = getAll conn :<|> getOne conn

getAll :: Connection -> Handler String
getAll conn = error "getAll"

getOne :: Connection -> Int -> Handler String
getOne conn = error "getOne"
