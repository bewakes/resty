module RestController where


data RestController a = RestController
    { queryset :: Int
    , serializer :: String
    , permissions :: String
    }
