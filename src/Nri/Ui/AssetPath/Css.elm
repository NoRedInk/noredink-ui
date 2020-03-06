module Nri.Ui.AssetPath.Css exposing (url)

{-| Helper for constructing commonly-used CSS functions
that reference assets.

@docs url

-}

import Nri.Ui.AssetPath as AssetPath exposing (Asset)


{-| Given an `Asset`, wrap its URL in a call to `url()`.
-}
url : Asset -> String
url asset =
    "url(" ++ AssetPath.url asset ++ ")"
