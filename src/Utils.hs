module Utils where

import Data.Text
import Data.ByteString
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.UTF8 as U
import qualified Data.CaseInsensitive as CI
import Data.CaseInsensitive(CI)

ci :: String -> CI ByteString
ci = CI.mk . U.fromString

toBytes :: Text -> ByteString
toBytes = TE.encodeUtf8
