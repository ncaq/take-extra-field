{-# LANGUAGE UndecidableInstances #-}
module Model where

import           Data.Aeson
import           Data.Text
import           Database.Esqueleto.PostgreSQL.JSON
import           Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    name Text
    age Int
    extraField (JSONB Value)
    UniqueUserName name
    deriving Show
|]
