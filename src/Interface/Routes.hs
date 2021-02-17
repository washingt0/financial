{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Interface.Routes
  ( baseServer
  , baseAPI
  ) where

import Servant
import Servant.API

import Interface.Bank.Bank as BB

-- import Middleware.UserInfo (fetchUserInfo, UserInfo)
type BaseAPI = "bank" :> BB.BankAPI

baseAPI :: Proxy BaseAPI
baseAPI = Proxy

baseServer :: Server BaseAPI
baseServer = bankServer
