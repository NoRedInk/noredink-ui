module AssetPath.Css exposing (url)

{-| Helper for constructing commonly-used CSS functions
that reference assets.

@docs url

-}

import AssetPath exposing (Asset)


{-| Given an `Asset`, wrap its URL in a call to `url()`.
-}
url : Asset -> String
url asset =
    "url(" ++ AssetPath.url asset ++ ")"
