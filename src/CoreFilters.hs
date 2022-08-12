{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module CoreFilters where

import Database.Persist
import Data.Text
import Text.Read(readMaybe)

class Filterable a where
    mkFilter :: Text -> Text -> [Filter a]
    mkFilter _ _ = []

intFilter :: forall e f. (PersistField f, Read f)
        => Text
        -> EntityField e f
        -> (EntityField e f -> f -> Filter e)
        -> [Filter e]
intFilter txt field op = case readMaybe @f (unpack txt) of
                      Just i -> [op field i]
                      Nothing -> []

icontains, contains :: EntityField e String -> Text -> [Filter e]
icontains field val = [Filter field (FilterValue $ mconcat ["%", unpack val, "%"]) (BackendSpecificFilter "ILIKE")]
contains field val = [Filter field (FilterValue $ mconcat ["%", unpack val, "%"]) (BackendSpecificFilter "LIKE")]
