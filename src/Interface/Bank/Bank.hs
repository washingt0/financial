{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Interface.Bank.Bank
  ( BankAPI
  , bankServer
  ) where

import Servant
import Servant.API

import Control.Monad.IO.Class
import Data.Pool
import Database.PostgreSQL.Simple
import Interface.Bank.Account.Account

import Data.Aeson
import GHC.Generics

data BankData =
  BankData
    { bankID :: String
    , bankCountryName :: String
    , bankCountryCode :: String
    , bankName :: String
    , bankCode :: String
    }
  deriving (Show, Generic)

instance ToJSON BankData

type BankAPI
   = "account" :> AccountAPI :<|> "country" :> Capture "countryCode" String :> Get '[ JSON] [BankData] :<|> Capture "id" String :> Get '[ JSON] String

bankServer :: Pool Connection -> Server BankAPI
bankServer p = accountServer p :<|> getAll p :<|> getOne p

getAll :: Pool Connection -> String -> Handler [BankData]
getAll pool countryCode = do
  liftIO . withResource pool $ \c -> fetchBanks c countryCode

fetchBanks :: Connection -> String -> IO [BankData]
fetchBanks c cc =
  map
    (\(id, ccName, ccCode, name, code) ->
       BankData
         { bankID = id
         , bankCountryName = ccName
         , bankCountryCode = ccCode
         , bankName = name
         , bankCode = code
         }) <$>
  query
    c
    "SELECT TB.id::TEXT, TC.name, TC.code, TB.name, TB.code \
    \FROM public.t_bank TB \
    \INNER JOIN public.t_country TC ON TC.id = TB.country_id \
    \WHERE TC.code = ?::TEXT"
    (Only cc)

getOne :: Pool Connection -> String -> Handler String
getOne = error "getOne"
