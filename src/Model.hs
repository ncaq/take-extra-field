{-# LANGUAGE UndecidableInstances #-}
module Model where

import Data.Text
import Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    name Text
    age Int
    UniqueUserName name
    deriving Show

UserExtraInt
    userId UserId
    name Text
    value Int
    UniqueUserIdName userId name
    deriving Show
|]
