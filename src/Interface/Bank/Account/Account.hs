{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Interface.Bank.Account.Account
  ( AccountAPI
  , accountServer
  ) where

import Servant
import Servant.API

type AccountAPI
   = Get '[ JSON] String :<|> Capture "id" Int :> Get '[ JSON] String

accountServer :: Server AccountAPI
accountServer = getAll :<|> getOne

getAll :: Handler String
getAll = error "getAll"

getOne :: Int -> Handler String
getOne = error "getOne"
