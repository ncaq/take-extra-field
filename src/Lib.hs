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
import           Data.Pool
import qualified Data.Text                          as T
import           Database.Esqueleto.PostgreSQL.JSON
import           Database.Persist.Postgresql
import           Model
import           System.Random

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
  seedJSON

-- | とりあえず10万件。
seedSize :: Int
seedSize = 100000

-- -- | ランダムなデータを挿入。
-- seedRelation :: ReaderT SqlBackend (NoLoggingT (ResourceT IO)) ()
-- seedRelation = do
--   let -- シード固定の疑似乱数値を挿入。
--     ageGen = mkStdGen 8591
--     costGen = mkStdGen 8198
--     randomAges = take seedSize $ randomRs (18, 65) ageGen
--     randomCosts = take seedSize $ randomRs (0, 100) costGen
--     userArgs = zip3 [(1 :: Int) .. 1000000] randomAges randomCosts
--   mapConcurrently_ insertUser userArgs

-- -- | 抽象から具体的なデータを挿入。
-- -- userIdが必要なので`insertMany`は使えない。
-- insertUser :: (BaseBackend backend ~ SqlBackend, MonadIO m, PersistStoreWrite backend, Show a) => (a, Int, Int) -> ReaderT backend m ()
-- insertUser (i, age, cost) = do
--   userId <- insert $ User ("User" <> T.pack (show i)) age
--   insert_ $ UserExtraInt userId "cost" cost

seedJSON :: ReaderT SqlBackend (NoLoggingT (ResourceT IO)) ()
seedJSON = do
  let -- シード固定の疑似乱数値を挿入。
    ageGen = mkStdGen 8591
    costGen = mkStdGen 8198
    randomAges = take seedSize $ randomRs (18 :: Int, 65) ageGen
    randomCosts = (\x -> JSONB (object ["cost" .= x])) <$> take seedSize (randomRs (0 :: Int, 100) costGen)
    ids = (\i -> "User" <> T.pack (show i)) <$> [(1 :: Int) .. 1000000]
    userArgs = zip3 ids randomAges randomCosts
  insertMany_ $ (\(i, a, c) -> User i a c) <$> userArgs

benchLookup :: IO ()
benchLookup = do
  pool <- getPool
  defaultMain
    [ bgroup "lookupUsersJson"
      [ bench "users 1-10" $ whnfIO $ runPoolDB pool $ lookupUsersJson (toSqlKey <$> [1..10])
      , bench "users 500000-500010" $ whnfIO $ runPoolDB pool $ lookupUsersJson (toSqlKey <$> [500000..500010])
      , bench "users 999990-1000000" $ whnfIO $ runPoolDB pool $ lookupUsersJson (toSqlKey <$> [999990..1000000])
      ]
    ]

-- lookupUsersRelation :: (BaseBackend backend ~ SqlBackend, PersistQueryRead backend, MonadIO m) => [UserId] -> ReaderT backend m ([Entity User], [Entity UserExtraInt])
-- lookupUsersRelation userIds = do
--   users <- selectList [UserId <-. userIds] []
--   costs <- selectList [UserExtraIntUserId <-. userIds, UserExtraIntName ==. "cost"] []
--   return (users, costs)

lookupUsersJson :: (BaseBackend backend ~ SqlBackend, PersistQueryRead backend, MonadIO m) => [UserId] -> ReaderT backend m [Entity User]
lookupUsersJson userIds = selectList [UserId <-. userIds] []
