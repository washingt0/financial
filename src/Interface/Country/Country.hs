{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}

module Interface.Country.Country (

) where

import GHC.Generics
import Data.Aeson
import Servant
import Servant.API

import qualified Database.Database as DB

data CountryData = CountryData {
    countryDataID :: String
    , countryName :: String
    , countryCurrency :: String
} deriving (Show, Generic)

instance ToJSON CountryData

type CountryAPI = Get '[ JSON] [CountryData]

countryServer :: Server CountryAPI
countryServer = getAll

getAll :: Handler [CountryData]
getAll = error ""