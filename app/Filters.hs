{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Filters where

import CoreFilters
import Models
import Quasi

[filterable|
Person
    name PersonName String
    age PersonAge Int
Car
    model CarModel String
|]
