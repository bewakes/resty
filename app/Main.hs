{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Wai
import Network.Wai.Handler.Warp(run)

import Core
import Models

import RestHandlers

import Data.Text
import Database.Persist.Sqlite

dbName :: Text
dbName = "test.db"


-- type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
app :: Text -> Application
app = routerToApplication myAppRouter

myAppRouter :: Router
myAppRouter path =
    case path of
      (_, "users":_) -> entityHandler @Person path
      (_, "cars":_) -> entityHandler @Car path
      _ -> notFound


port :: Int
port = 3333

main :: IO ()
main = do
    print $ "running server on port " ++ show port ++ "..."
    runSqlite dbName $ do
        runMigration migrateAll
    run port $ app dbName
