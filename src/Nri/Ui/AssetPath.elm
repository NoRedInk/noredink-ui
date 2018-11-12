module Nri.Ui.AssetPath exposing (Asset(..), url)

{-| Helpers for dealing with assets.

@docs Asset, url

-}


{-| -}
type Asset
    = Asset String


{-| -}
url : Asset -> String
url (Asset url_) =
    url_
