{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Interface.Routes
  ( baseServer
  , baseAPI
  , BaseAPI
  ) where

import Servant
import Servant.API

import Interface.Bank.Bank as BB
import Data.Pool
import Database.PostgreSQL.Simple

import Control.Monad.IO.Class

-- import Middleware.UserInfo (fetchUserInfo, UserInfo)
type BaseAPI = "bank" :> BB.BankAPI

baseAPI :: Proxy BaseAPI
baseAPI = Proxy

baseServer :: Connection -> Server BaseAPI
baseServer = bankServer
