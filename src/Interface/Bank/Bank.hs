{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Interface.Bank.Bank
  ( BankAPI
  , bankServer
  ) where

import Servant
import Servant.API

import Interface.Bank.Account.Account
import Data.Pool
import Database.PostgreSQL.Simple ( Connection )
import Control.Monad.IO.Class

type BankAPI
   = "account" :> AccountAPI :<|> Get '[ JSON] String :<|> Capture "id" Int :> Get '[ JSON] String

bankServer :: Connection -> Server BankAPI
bankServer conn = accountServer conn :<|> getAll conn :<|> getOne conn

getAll :: Connection -> Handler String
getAll = error "getAll"

getOne :: Connection -> Int -> Handler String
getOne = error "getOne"
