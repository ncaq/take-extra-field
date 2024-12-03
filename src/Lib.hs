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
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Database.Persist
import           Database.Persist.Postgresql
import           Model
import           System.Random
import           UnliftIO.Async

someFunc :: IO ()
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
  mapConcurrently_ (\(i, age, cost) -> insertUser ("User" <> T.pack (show i)) age cost) userArgs

-- | 別テーブル方式とJSON方式で実装が異なる。
insertUser :: (BaseBackend backend ~ SqlBackend, MonadIO m, PersistStoreWrite backend) => Text -> Int -> Int -> ReaderT backend m ()
insertUser name age cost = do
  userId <- insert $ User name age
  insert_ $ UserExtraInt userId "cost" cost

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
lookupUser :: (BaseBackend backend ~ SqlBackend, MonadIO m,  PersistUniqueRead backend) => UserId -> ReaderT backend m (Maybe User, Maybe (Entity UserExtraInt))
lookupUser userId = do
  user <- get userId
  cost <- getBy $ UniqueUserIdName userId "cost"
  return (user, cost)
