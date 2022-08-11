{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Core where

import Utils(ci, toBytes, serializeMany)

import Data.Aeson
import Control.Exception(SomeException(..), catch)
import Control.Monad.Logger
import Data.Maybe
import Data.Tuple.Extra
import Control.Monad.Reader
import Control.Monad.Except
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.UTF8 as U
import Control.Monad.State
import Data.Text(Text, unpack, pack)
import Text.Read(readMaybe)
import Data.Map.Strict(Map)
import qualified Data.Map as M
import qualified Data.Text.Encoding as TE
import Network.Wai
    ( Request
    , Response
    , queryString
    , responseLBS
    , strictRequestBody
    , Application
    , pathInfo
    , requestMethod
    )
import qualified Data.CaseInsensitive as CI
import Network.Wai.Parse
import Network.HTTP.Types (
    ResponseHeaders, Status, status200, status302, status400,
    hLocation, internalServerError500, HeaderName
  )
import Database.Persist.Sqlite

type Params = Map Text [Text]

data RequestData = RequestData
    { waiReq :: Request
    , queryParams :: Params
    , postParams :: Params
    , connPool :: ConnectionPool
    }

type Decoder a = B.ByteString -> Result a

data ResponseState = ResponseState
    { resStatus :: Status
    , resHeaders :: ResponseHeaders
    , content :: BL.ByteString
    }

makePool :: Text -> IO ConnectionPool
makePool connStr = runStdoutLoggingT $ createSqlitePool connStr 5

data HandlerResult =
      Redirect B.ByteString -- Redirect to a URL
    | ResponseComplete -- Send the response
    | HandlerError B.ByteString -- Send an internal error response
    deriving (Show, Eq)

type Handler a = ExceptT HandlerResult (ReaderT RequestData (StateT ResponseState IO)) a

type URLPath = [Text]

data Method = GET | POST | PUT | DELETE | OPTIONS | HEAD
    deriving (Show, Read)

type Router = (Method, URLPath) -> Handler ()

createRequestData :: Request -> ConnectionPool -> [Param] -> RequestData
createRequestData req pool pParams = RequestData
            { waiReq = req
            , queryParams = toMap $ second (fromMaybe B.empty) <$> queryString req
            , postParams = toMap pParams
            , connPool = pool
            }

runHandler :: Text -> Request -> Handler () -> IO Response
runHandler dbName req h = do
    -- parseRequestBody :: BackeEnd y -> Request -> IO ([Param], [File y])
    -- type Param = (ByteString, ByteString)
    (pParams, _) <- parseRequestBody lbsBackEnd req
    pool <- liftIO $ makePool dbName
    let initRes = ResponseState status200 [] BL.empty
        reqD = createRequestData req pool pParams
    (result, res) <- runStateT (runReaderT (runExceptT h) reqD) initRes
    let hdrs = resHeaders res
    case result of
               Left ResponseComplete -> return $ responseLBS (resStatus res) hdrs (content res)
               Left (Redirect url) -> return $ responseLBS status302 ((hLocation, url) : hdrs) BL.empty
               Left (HandlerError msg) -> return $ responseLBS internalServerError500 hdrs (BL.fromStrict msg)
               Right _ -> error "Not Handled"

_dec = TE.decodeUtf8

toMap :: [(B.ByteString, B.ByteString)] -> Params
toMap = M.unionsWith (++) . map (\(x, y) -> M.singleton (_dec x) [_dec y])


postParam :: Text -> Handler Text
postParam name = do
    pms <- asks postParams
    lookupParam name pms

queryParam :: Text -> Handler Text
queryParam name = do
    qps <- asks queryParams
    lookupParam name qps

lookupParam :: Text -> Params -> Handler Text
lookupParam name params =
    case M.lookup name params of
      Just [v] -> return v
      _ -> throwError $ HandlerError $ B.concat [U.fromString "Missing or duplicate parameter: ", TE.encodeUtf8 name]

body :: Handler BL.ByteString
body = do
    req <- asks waiReq
    let bs = strictRequestBody req
    liftIO bs

redirect :: B.ByteString -> Handler a
redirect = throwError . Redirect

-- change state to change status
status :: Status -> Handler ()
status s = modify $ \rs -> rs {resStatus = s }

-- Responses
text :: Text -> Handler ()
text t =
    setContentType (BL.fromStrict . toBytes $ "text/plain; charset=utf-8") >>
    (rawBytes . BL.fromStrict $ TE.encodeUtf8 t)

rawBytes :: BL.ByteString -> Handler ()
rawBytes b = modify (\rs -> rs { content = b }) >> throwError ResponseComplete

setHeader :: HeaderName -> BL.ByteString -> Handler ()
setHeader name value = modify $ \rs -> rs { resHeaders = (name, BL.toStrict value) : resHeaders rs }

setContentType :: BL.ByteString -> Handler ()
setContentType = setHeader (ci "Content-Type")

routerToApplication :: Router -> Text -> Application
routerToApplication route dbName = \req respond -> do
    let method = fromMaybe GET $ readMaybe @Method (unpack . TE.decodeUtf8 $ requestMethod req)
        normalHandle = do
            resp <- runHandler dbName req (route (method, pathInfo req))
                `catch` \(e::SomeException) -> do
                    print $ show e
                    return $ responseLBS internalServerError500 [] BL.empty
            respond resp
    normalHandle

withDeserializer :: (FromJSON a) => (a -> Handler ()) -> Handler ()
withDeserializer handler = do
    mval <- decode <$> body :: (Handler (Maybe Value))
    case mval of
      Nothing -> do
          status status400 -- TODO: Show what errors/what fields missing
          text "Not a JSON data"
      Just val -> do
          obj <- fromJSON <$> pure val
          case obj of
            Success a -> handler a
            Error s -> do
                status status400
                text (pack s)

withEntitySerializer :: (ToJSON a, ToBackendKey SqlBackend a) => Handler [Entity a] -> Handler ()
withEntitySerializer h = do
    entityItems <- h
    status status200
    setContentType "application/json"
    rawBytes $ encode $ serializeMany entityItems
