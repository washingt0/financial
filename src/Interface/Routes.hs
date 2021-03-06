{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Interface.Routes
  ( baseServer
  , baseAPI
  , BaseAPI
  ) where

import Servant
import Servant.API

import Data.Pool
import Database.PostgreSQL.Simple
import Interface.Bank.Bank as BB
import qualified Interface.Country.Country as CC

import Control.Monad.IO.Class

-- import Middleware.UserInfo (fetchUserInfo, UserInfo)
type BaseAPI = "country" :> CC.CountryAPI

baseAPI :: Proxy BaseAPI
baseAPI = Proxy

baseServer :: Pool Connection -> Server BaseAPI
baseServer = CC.countryServer
