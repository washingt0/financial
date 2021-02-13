{-# LANGUAGE OverloadedStrings #-}

module Interface.Bank.Account.Account
  ( router
  ) where

import Snap.Core

router :: Snap ()
router = route [("", getMyAccounts)]

getMyAccounts :: Snap ()
getMyAccounts = writeBS "AAAAA"
