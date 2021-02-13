{-# LANGUAGE OverloadedStrings #-}

module Interface.Routes (
    routingTree
) where

import Interface.Bank.Bank as BB
import Data.Bifunctor (first)
import Snap.Core

routingTree :: Snap()
routingTree = 
    route [
        ("bank", BB.router)
    ]