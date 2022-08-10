{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
module RestHandlers where

import Network.HTTP.Types(status201)

import Data.Aeson
import Database.Persist
import Database.Persist.Sql
import Data.Text(pack )

import Db
import Core

addHandler :: (FromJSON a, PersistEntityBackend a ~ SqlBackend, ToBackendKey SqlBackend a) => a -> Handler ()
addHandler obj = do
    uid <- insertDb obj
    status status201
    text $ "Successful create. The id is: " <> (pack . show . fromSqlKey) uid

listHandler :: (ToJSON a, PersistEntityBackend a ~ SqlBackend, PersistEntity a) => Handler [Entity a]
listHandler = do
    users <- (selectDb [] [])
    return users

notFound :: Handler ()
notFound = undefined

entityHandler :: forall a. (FromJSON a, ToJSON a, ToBackendKey SqlBackend a, PersistEntityBackend a ~ SqlBackend, PersistEntity a) => (Method, URLPath) -> Handler ()
entityHandler path = case path of
    (POST, ["users"]) -> withDeserializer @a (addHandler)
    (GET, ["users"]) -> withEntitySerializer @a (listHandler)
