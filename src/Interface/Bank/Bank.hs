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

bankServer :: Server BankAPI
bankServer = accountServer :<|> getAll :<|> getOne 

getAll :: Handler String
getAll = error "getAll"

getOne :: Int -> Handler String
getOne = error "getOne"
