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
import           Data.Pool
import qualified Data.Text                    as T
import           Database.Persist.Postgresql
import           Model
import           System.Random
import           UnliftIO

someFunc :: IO ()
-- someFunc = runSeed
someFunc = benchLookup

connStr :: ConnectionString
connStr = "host=localhost port=5432 user=test dbname=test password=test"

connSize :: Int
connSize = 1

runLogDB :: MonadUnliftIO m => ReaderT SqlBackend (NoLoggingT (ResourceT IO)) a -> m a
runLogDB action = runStderrLoggingT $ withPostgresqlPool connStr 1 (liftIO . runSqlPersistMPool action)

getPool :: IO (Pool SqlBackend)
getPool = runNoLoggingT $ createPostgresqlPool connStr connSize

runPoolDB :: (MonadIO m, BackendCompatible SqlBackend backend) => Pool backend -> ReaderT backend (NoLoggingT (ResourceT IO)) a -> m a
runPoolDB pool action = runNoLoggingT $ liftIO $ runSqlPersistMPool action pool

runSeed :: IO ()
runSeed = runLogDB $ do
  runMigration migrateAll
  seedRelation

-- | とりあえず10万件。
seedSize :: Int
seedSize = 100000

-- | ランダムなデータを挿入。
seedRelation :: ReaderT SqlBackend (NoLoggingT (ResourceT IO)) ()
seedRelation = do
  let -- シード固定の疑似乱数値を挿入。
    ageGen = mkStdGen 8591
    costGen = mkStdGen 8198
    randomAges = take seedSize $ randomRs (18, 65) ageGen
    randomCosts = take seedSize $ randomRs (0, 100) costGen
    userArgs = zip3 [(1 :: Int) .. 1000000] randomAges randomCosts
  mapConcurrently_ insertUser userArgs

-- | 抽象から具体的なデータを挿入。
-- userIdが必要なので`insertMany`は使えない。
insertUser :: (BaseBackend backend ~ SqlBackend, MonadIO m, PersistStoreWrite backend, Show a) => (a, Int, Int) -> ReaderT backend m ()
insertUser (i, age, cost) = do
  userId <- insert $ User ("User" <> T.pack (show i)) age
  insert_ $ UserExtraInt userId "cost" cost

benchLookup :: IO ()
benchLookup = do
  pool <- getPool
  defaultMain
    [ bgroup "lookupUserRelation"
      [ bench "user 1" $ whnfIO $ runPoolDB pool $ lookupUserRelation (toSqlKey 1)
      , bench "user 500000" $ whnfIO $ runPoolDB pool $ lookupUserRelation (toSqlKey 500000)
      , bench "user 1000000" $ whnfIO $ runPoolDB pool $ lookupUserRelation (toSqlKey 1000000)
      ]
    ]

lookupUserRelation :: (BaseBackend backend ~ SqlBackend, MonadIO m, PersistUniqueRead backend) => UserId -> ReaderT backend m (Maybe User, Maybe (Entity UserExtraInt))
lookupUserRelation userId = do
  user <- get userId
  cost <- getBy $ UniqueUserIdName userId "cost"
  return (user, cost)
  -- get userId
