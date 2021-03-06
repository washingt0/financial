{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Interface.Country.Country
  ( CountryAPI
  , countryServer
  ) where

import Data.Aeson
import GHC.Generics
import Servant
import Servant.API

import qualified Database.Database as DB

import Control.Monad
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Pool (Pool, withResource)

import Database.PostgreSQL.Simple

type CountryName = String

data CountryData =
  CountryData
    { countryID :: String
    , countryName :: String
    , countryCurrency :: String
    }
  deriving (Show, Generic)

instance ToJSON CountryData

type CountryAPI = Get '[ JSON] [CountryData]

countryServer :: Pool Connection -> Server CountryAPI
countryServer = getAll

getAll :: Pool Connection -> Handler [CountryData]
getAll pool = do
  liftIO $ withResource pool $ \c -> fetchCountries c

fetchCountries :: Connection -> IO [CountryData]
fetchCountries c =
  map retrieveCountries <$>
  query_
    c
    "SELECT id::TEXT, name, currency FROM public.t_country WHERE deleted_at IS NULL"

retrieveCountries :: (String, String, String) -> CountryData
retrieveCountries (id, name, currency) =
  CountryData {countryID = id, countryName = name, countryCurrency = currency}
