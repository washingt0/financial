module Database.Exceptions
  ( MigrationException
  ) where

import Control.Exception
import Data.Typeable

data MigrationException =
  InvalidMigrationVersion
  deriving (Show, Typeable)

instance Exception MigrationException
