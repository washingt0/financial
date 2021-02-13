{-# LANGUAGE OverloadedStrings #-}

module Interface.Bank.Bank
  ( router
  ) where

import Snap.Core

import qualified Data.ByteString.Lazy as B
import qualified Interface.Bank.Account.Account as A

router :: Snap ()
router = route [("", getAll), (":id", getOne), (":id/account", A.router)]

getAll :: Snap ()
getAll = writeBS "GetAll bank accounts"

getOne :: Snap ()
getOne = writeBS "GetOne bank "
