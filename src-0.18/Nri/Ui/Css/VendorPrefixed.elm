module Nri.Ui.Css.VendorPrefixed exposing (property, value, complexProperty)

{-| Vendor prefixed css properties.

@docs property, value, complexProperty

-}

import Css


{-| Css vendor prefixes
-}
prefixes : List String
prefixes =
    [ "-webkit-", "-moz-", "-o-", "-ms-", "" ]


{-| Same as Css.property but vendor prefixed.
-}
property : String -> String -> Css.Style
property prop value =
    prefixes
        |> List.map
            (\prefix ->
                Css.property (prefix ++ prop) value
            )
        |> Css.batch


{-| Same as Css.property but vendor prefixed.
-}
value : String -> String -> Css.Style
value prop value =
    prefixes
        |> List.map
            (\prefix ->
                Css.property prop (prefix ++ value)
            )
        |> Css.batch


{-| Used to build more complex Css styles
-}
complexProperty : (String -> Css.Style) -> Css.Style
complexProperty buildProp =
    prefixes
        |> List.map buildProp
        |> Css.batch
