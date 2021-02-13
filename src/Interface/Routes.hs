{-# LANGUAGE OverloadedStrings #-}

module Interface.Routes
  ( routingTree
  ) where

import Data.Bifunctor (first)
import Interface.Bank.Bank as BB
import Snap.Core

routingTree :: Snap ()
routingTree = route [("bank", BB.router)]
