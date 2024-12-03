module Lib
    ( someFunc
    , runSeed
    , benchLookup
    ) where

import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import           Criterion.Main
import           Data.Aeson
import qualified Data.Text                          as T
import           Database.Esqueleto.PostgreSQL.JSON
import           Database.Persist
import           Database.Persist.Postgresql
import           Model
import           System.Random

someFunc :: IO ()
-- someFunc = runSeed
someFunc = benchLookup

connStr :: ConnectionString
connStr = "host=localhost port=5432 user=test dbname=test password=test"

runLogDB :: MonadUnliftIO m => ReaderT SqlBackend (NoLoggingT (ResourceT IO)) a -> m a
runLogDB action = runStderrLoggingT $ withPostgresqlPool connStr 32 (liftIO . runSqlPersistMPool action)

runDB :: MonadUnliftIO m => ReaderT SqlBackend (NoLoggingT (ResourceT IO)) a -> m a
runDB action = runNoLoggingT $ withPostgresqlPool connStr 32 (liftIO . runSqlPersistMPool action)

runSeed :: IO ()
runSeed = runLogDB $ do
  runMigration migrateAll
  seed

-- | とりあえず10万件。
seedSize :: Int
seedSize = 100000

-- | ランダムなデータを挿入。
seed :: ReaderT SqlBackend (NoLoggingT (ResourceT IO)) ()
seed = do
  let -- シード固定の疑似乱数値を挿入。
    ageGen = mkStdGen 8591
    costGen = mkStdGen 8198
    randomAges = take seedSize $ randomRs (18, 65) ageGen
    randomCosts = take seedSize $ randomRs (0, 100) costGen
    userArgs = zip3 [(1 :: Int) .. 1000000] randomAges randomCosts
    users :: [User] = mkUser <$> userArgs
  insertMany_ users

-- | 抽象から具体的なデータ生成。
-- 別テーブル方式とJSON方式で実装が異なる。
mkUser :: (Int, Int, Int) -> User
mkUser (i, age, cost) = User ("User" <> T.pack (show i)) age (JSONB (object ["cost" .= cost]))

benchLookup :: IO ()
benchLookup =
  defaultMain
  [
    bgroup "lookupUser"
    [ bench "user 1" $ whnfIO $ runDB $ lookupUser (toSqlKey 1)
    , bench "user 500000" $ whnfIO $ runDB $ lookupUser (toSqlKey 500000)
    , bench "user 1000000" $ whnfIO $ runDB $ lookupUser (toSqlKey 1000000)
    ]
  ]

-- | 別テーブル方式とJSON方式で実装が異なる。
lookupUser :: (BaseBackend backend ~ SqlBackend, MonadIO m,  PersistUniqueRead backend) => UserId -> ReaderT backend m (Maybe User)
lookupUser = get
