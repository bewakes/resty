{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
module RestHandlers where

import Network.HTTP.Types(status201, status200, status404, status405)

import Data.Aeson
import Data.Int(Int64)
import Database.Persist
import Database.Persist.Sql
import Data.Text(pack, unpack)
import Text.Read(readMaybe)

import Db
import Core
import Utils(serialize)

addHandler :: (ToJSON a, FromJSON a, PersistEntityBackend a ~ SqlBackend, ToBackendKey SqlBackend a) => a -> Handler ()
addHandler obj = do
    uid <- insertDb obj
    status status201
    setContentType "application/json"
    rawBytes $ encode $ serialize (Entity uid obj)

listHandler :: (ToJSON a, PersistEntityBackend a ~ SqlBackend, PersistEntity a) => Handler [Entity a]
listHandler = do
    items <- (selectDb [] [])
    return items

retrieveHandler :: (ToBackendKey SqlBackend a, ToJSON a, PersistEntityBackend a ~ SqlBackend, PersistEntity a) => Key a -> Handler ()
retrieveHandler key = do
    mItem <- getDb key
    case mItem of
      (Just item) -> do
        status status200
        setContentType "application/json"
        rawBytes $ encode $ serialize (Entity key item)
      Nothing -> do
          status status404
          rawBytes $ encode $ object []

notFound :: Handler ()
notFound = undefined

send405 :: Handler ()
send405 = do
    status status405
    text ""

entityHandler :: forall a. (FromJSON a, ToJSON a, ToBackendKey SqlBackend a, PersistEntityBackend a ~ SqlBackend, PersistEntity a) => (Method, URLPath) -> Handler ()
entityHandler path = case path of
    (POST, [_]) -> withDeserializer @a (addHandler)
    (GET, [_]) -> withEntitySerializer @a (listHandler)
    (GET, _:id:[]) -> do
        let mInt = readMaybe @Int64 (unpack id)
        case mInt of
          Nothing -> send405
          (Just int) -> retrieveHandler $ toSqlKey @a int
    _ -> send405
