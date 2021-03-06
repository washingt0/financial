{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Interface.Bank.Bank
  ( BankAPI
  , bankServer
  ) where

import Servant
import Servant.API

import Control.Monad.IO.Class
import Data.Pool
import Database.PostgreSQL.Simple (Connection)
import Interface.Bank.Account.Account

type BankAPI
   = "account" :> AccountAPI :<|> Get '[ JSON] String :<|> Capture "id" Int :> Get '[ JSON] String

bankServer :: Server BankAPI
bankServer = accountServer :<|> getAll :<|> getOne

getAll :: Handler String
getAll = error "getAll"

getOne :: Int -> Handler String
getOne = error "getOne"
