{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
module RestHandlers where

import Network.HTTP.Types(status201, status200, status404, status405, status204)

import Control.Monad.Reader(liftIO, asks)
import Data.Aeson
import Data.Int(Int64)
import Data.Map(toList)
import Data.Text(pack, unpack, Text)
import Database.Persist
import Database.Persist.Sql
import Text.Read(readMaybe)

import Core
import CoreFilters
import Db
import Utils(serialize)

addHandler :: (ToJSON a, FromJSON a, PersistEntityBackend a ~ SqlBackend, ToBackendKey SqlBackend a) => a -> Handler ()
addHandler obj = do
    uid <- insertDb obj
    status status201
    setContentType "application/json"
    rawBytes $ encode $ serialize (Entity uid obj)

getFilterParams :: forall a.(Filterable a, PersistEntityBackend a ~ SqlBackend, PersistEntity a) => Handler [Filter a]
getFilterParams = do
    qps <- asks queryParams
    let listqps = map (\(a, x:xs) -> (a, x)) $ filter (\(a, b) -> not (null b)) $ toList qps
        filters = mconcat $ map (uncurry mkFilter) listqps
    pure filters

listHandler :: (Filterable a, ToJSON a, PersistEntityBackend a ~ SqlBackend, PersistEntity a) => Handler [Entity a]
listHandler = do
    filterParams <- getFilterParams
    runDb $ selectList filterParams []

retrieveHandler :: (ToBackendKey SqlBackend a, ToJSON a, PersistEntityBackend a ~ SqlBackend, PersistEntity a) => Key a -> Handler ()
retrieveHandler key = do
    mItem <- runDb $ get key
    case mItem of
      (Just item) -> do
        status status200
        setContentType "application/json"
        rawBytes $ encode $ serialize (Entity key item)
      Nothing -> send404

deleteHandler :: forall a.(ToBackendKey SqlBackend a, ToJSON a, PersistEntityBackend a ~ SqlBackend, PersistEntity a) => Key a -> Handler ()
deleteHandler key = do
    mItem <- runDb $ get key
    case mItem of
      (Just item) -> do
          runDb $ delete key
          status status204
          rawBytes ""
      Nothing -> send404

notFound :: Handler ()
notFound = send404

send404 :: Handler ()
send404 = do
    status status404
    rawBytes $ encode $ object ["error" .= ("Requested entity not Found" :: Text)]

send405 :: Handler ()
send405 = do
    status status405
    text ""

withIntToSqlKey
    :: forall a.(Filterable a, FromJSON a, ToJSON a, ToBackendKey SqlBackend a, PersistEntityBackend a ~ SqlBackend, PersistEntity a)
    => Text
    -> (Key a -> Handler())
    -> Handler()
withIntToSqlKey idStr h = do
        let mInt = readMaybe @Int64 (unpack idStr)
        case mInt of
          Nothing -> send405
          Just int -> h $ toSqlKey @a int

entityCRUDHandler
    :: forall a. (Filterable a, FromJSON a, ToJSON a, ToBackendKey SqlBackend a, PersistEntityBackend a ~ SqlBackend, PersistEntity a)
    => (Method, URLPath)
    -> Handler ()
entityCRUDHandler path = case path of
    (POST, [_]) -> withDeserializer @a addHandler
    (GET, [_]) -> withEntitySerializer @a listHandler
    (DELETE, [_,id]) -> withIntToSqlKey @a id deleteHandler
    (GET, [_, id]) -> withIntToSqlKey @a id retrieveHandler
    _ -> send405
