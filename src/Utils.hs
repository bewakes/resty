{-# LANGUAGE FlexibleContexts #-}
module Utils where

import Data.Aeson
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import Data.Text
import Database.Persist
import Database.Persist.Sql
import Data.ByteString
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.UTF8 as U
import qualified Data.CaseInsensitive as CI
import Data.CaseInsensitive(CI)

ci :: String -> CI ByteString
ci = CI.mk . U.fromString

toBytes :: Text -> ByteString
toBytes = TE.encodeUtf8

mergeValues :: [Value] -> Value
mergeValues = Object . HM.unions . Prelude.map (\(Object x) -> x)

serialize ::  (ToBackendKey SqlBackend v, ToJSON v) => Entity v -> Value
serialize (Entity k obj) =  mergeValues [object ["id" .= fromSqlKey k], toJSON obj ]

serializeMany :: (ToBackendKey SqlBackend v, ToJSON v) => [Entity v] -> Value
serializeMany objs = object
    [ "count" .= Prelude.length objs
    , "items" .= Array (V.fromList $ Prelude.map serialize objs)
    ]


