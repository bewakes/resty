{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Db where

import Control.Monad.Reader (MonadIO, MonadReader, asks, liftIO)
import Control.Monad.Logger
import Data.Text
import Database.Persist
import Database.Persist.Sqlite

import Core

runDb :: SqlPersistT IO b -> Handler b
runDb query = do
    pool <- asks connPool
    liftIO $ runSqlPool query pool

selectDb :: (PersistEntity b, PersistEntityBackend b ~ SqlBackend) =>
    [Filter b] -> [SelectOpt b] -> Handler [Entity b]
selectDb filters opts = runDb $ selectList filters opts

getDb :: (PersistEntity b, PersistEntityBackend b ~ SqlBackend) => Key b -> Handler (Maybe b)
getDb key = runDb $ get key

insertDb :: (PersistEntity b, PersistEntityBackend b ~ SqlBackend) =>
    b -> Handler (Key b)
insertDb e = runDb $ insert e

_testRunPersistent :: IO ()
_testRunPersistent = do
    pool <- makePool "test.db"
    print ""
    -- runDb 
    {-
    runSqlite ":memory:" $ do
        insert $ Person "bibek" 10
        liftIO $ print "done"
    -}
