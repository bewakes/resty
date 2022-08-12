{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Filters where

import Text.Read(readMaybe)
import Database.Persist
import Data.Text(unpack, Text, concat)

import Core
import CoreFilters
import Models
import Quasi

{-
instance Filterable Person where
    mkFilter "name" x = [PersonName ==. (unpack x)]
    mkFilter "name__icontains" x = icontains PersonName x
    mkFilter "name__contains" x = contains PersonName x
    mkFilter "age" x = intFilter x PersonAge (==.)
    mkFilter "age__lt" x = intFilter x PersonAge (<.)
    mkFilter "age__lte" x = intFilter x PersonAge (<=.)
    mkFilter "age__gt" x = intFilter x PersonAge (>.)
    mkFilter "age__gte" x = intFilter x PersonAge (>=.)
    mkFilter _ _ = []
-}

[filterable|
Person
    name PersonName String
    age PersonAge Int
Car
    model CarModel String
|]
