module AssetPath exposing (..)


type Asset
    = Asset String


url : Asset -> String
url (Asset url) =
    url
