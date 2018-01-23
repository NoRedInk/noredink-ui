module AssetPath exposing (Asset(Asset), url)

{-| Helpers for dealing with assets.

@docs Asset, url

-}


{-| -}
type Asset
    = Asset String


{-| -}
url : Asset -> String
url (Asset url) =
    url
