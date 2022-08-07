module Handlers where

import Network.HTTP.Types(status201)

import Database.Persist
import Database.Persist.Sql
import Data.Text(pack )

import Db
import Core
import Models

addUserHandler :: Person -> Handler ()
addUserHandler user = do
    uid <- insertDb $ user
    status status201
    text $ "Successful create. Person id is: " <> (pack . show . fromSqlKey) uid

usersHandler :: Handler [Entity Person]
usersHandler = do
    users <- (selectDb [] [] :: Handler [Entity Person])
    return users


notFound :: Handler ()
notFound = undefined
