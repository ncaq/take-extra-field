{-# LANGUAGE UndecidableInstances #-}
module Model where

import           Data.Aeson
import           Data.Text
import           Database.Persist.Postgresql.JSON ()
import           Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    name Text
    age Int
    value Value
    UniqueUserName name
    deriving Show
|]
