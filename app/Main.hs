{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.HTTP.Types(status200)
import Network.Wai
import Network.Wai.Handler.Warp(run)

import Core
import Utils(ci)
import Db
import Models

import Handlers

import Data.Text
import qualified Data.CaseInsensitive as CI
import Data.CaseInsensitive(CI)
import Data.ByteString
import qualified Data.ByteString.UTF8 as U
import qualified Data.ByteString.Lazy.UTF8 as LU
import Database.Persist.Sqlite
import Database.Persist

dbName :: Text
dbName = "test.db"


-- type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
app :: Text -> Application
app = routerToApplication myAppRouter


myAppRouter :: Router
myAppRouter path =
    case path of
      (POST, ["users"]) -> addUserHandler
      (GET,["users"]) -> showUserHandler
      _ -> notFound


port :: Int
port = 3333

main :: IO ()
main = do
    print $ "running server on port " ++ show port ++ "..."
    runSqlite dbName $ do
        runMigration migrateAll
    run port $ app dbName
