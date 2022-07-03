{-# LANGUAGE OverloadedStrings #-}
module Handlers where

import Network.HTTP.Types(status200, status201)

import Data.Maybe(fromMaybe)
import Database.Persist
import Database.Persist.Sql
import Data.Text(pack, unpack)
import Text.Read(readMaybe)

import Db
import Core
import Models

addUserHandler :: Handler ()
addUserHandler = do
    name <- unpack <$> postParam "name"
    ageStr <- unpack <$> postParam "age"
    let age = fromMaybe 0 $ readMaybe @Int ageStr
    id <- insertDb $ Person name age
    status status201
    text $ "Successful create. Person id is: " <> (pack . show . fromSqlKey) id

showUserHandler :: Handler ()
showUserHandler = do
    users <- map entityVal <$> (selectDb [] [] :: Handler [Entity Person])
    status status200
    text $ "There are " <> (pack . show . length) users <> " users"




notFound :: Handler ()
notFound = undefined
