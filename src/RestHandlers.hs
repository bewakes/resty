{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module RestHandlers where

import Network.HTTP.Types(status201)

import Data.Aeson
import Database.Persist
import Database.Persist.Sql
import Data.Text(pack )

import Db
import Core

addHandler :: (FromJSON a, PersistEntityBackend a ~ SqlBackend, ToBackendKey SqlBackend a) => p a -> a -> Handler ()
addHandler _ obj = do
    uid <- insertDb obj
    status status201
    text $ "Successful create. The id is: " <> (pack . show . fromSqlKey) uid

listHandler :: (ToJSON a, PersistEntityBackend a ~ SqlBackend, PersistEntity a) => p a ->  Handler [Entity a]
listHandler _ = do
    users <- (selectDb [] [])
    return users


notFound :: Handler ()
notFound = undefined

entityHandler :: (FromJSON a, ToJSON a, ToBackendKey SqlBackend a, PersistEntityBackend a ~ SqlBackend, PersistEntity a) => p a -> (Method, URLPath) -> Handler ()
entityHandler pxy path = case path of
    (POST, ["users"]) -> withDeserializer (addHandler pxy)
    (GET, ["users"]) -> withEntitySerializer (listHandler pxy)
