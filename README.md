## resty
Rest APIs never got easier.

### Motivation
I've always been fascinated by the easiness of python, especially `django` and `django-rest-framework`.

This project is my attempt to create an easy-to-use rest framework and more importantly, to learn Haskell.

Please note that this project is still at its infancy.


### Installation
It is not currently available in stackage or in hackage. So you have to clone this repo.

### Usage
This assumes that your project uses [persistent](https://github.com/yesodweb/persistent) and [wai](https://github.com/yesodweb/wai).
Also make sure you have `OverloadedStrings` and `TypeApplications` language extensions enabled.

Say you have some persistent models defined as:
```
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person json
    name String
    age Int
    deriving Show
Car json
    color String
    make String
    model String
    deriving Show
|]
```
Import the required modules
```haskell
-- Thirdparty imports
import Network.Wai
import Network.Wai.Handler.Warp(run)
import Data.Text
import Database.Persist.Sqlite

-- This library specific imports
import Core
import CoreFilters
import RestHandlers
```

Define application. An application is just a wai application.
```haskell
-- type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived

app :: Text -> Application
app = routerToApplication myRouter
```

Define `myRouter`
```haskell
-- type Router = (Method, URLPath) -> Handler ()

myRouter :: Router
myRouter path =
    case path of
      (_, "users":_) -> entityCRUDHandler @Person path
      (_, "cars":_) -> entityCRUDHandler @Car path
      _ -> notFound
```
The `entityCRUDHandler` will generate handlers for GET and POST methods(for now). Custom handlers for routes can also be written.

In order to use your models as above, they must be instances of `Filterable` (from CoreFilters). For now just do the following. Will discuss in detail later.
```haskell
instance Filterable Person
instance Filterable Car
```

And run it in main
```haskell
port :: Int
port = 3333

dbName :: Text
dbName = "test.db"

main :: IO ()
main = do
    print $ "running server on port " ++ show port ++ "..."
    runSqlite dbName $ do
        runMigration migrateAll
    run port $ app dbName
```
That's it! You can then have the following apis being served.
```
GET /users
GET /users/<id>
POST /users

GET /cars
GET /cars/<id>
POST /cars
```
The list api will not have any filter capabilities now, for example: `GET /users?name=curry` will not filter users by name.
In order to do so the `Filterable` instance that we declared above must be extended as:
```haskell
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
```
Just make sure to import `Database.Persist`.

**Seems too cumbersome?**
The library also defines quasiquotes for deriving `Filterable` instances. The following is above's equivalent:
```haskell
import CoreFilters
import Quasi

[filterable|
Person
    name PersonName String
    age PersonAge Int
|]
```
Now you can have queries like: `GET /users?name__contains=bi&age__gte=25`.


### Documentation
to do

### TODO
- [x] `PUT` and `DELETE` handlers
- [ ] Exception Handling
- [ ] Authentication
- [ ] Tests !!!
- [ ] Custom customizable serializers. Currently only the persistent models are supported.
- [ ] Documentation
- [ ] Middlewares
- [ ] Other cool features as required.
