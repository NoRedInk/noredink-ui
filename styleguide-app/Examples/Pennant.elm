module Examples.Pennant exposing (example)

{-|

@docs example

-}

import Category exposing (Category(..))
import Css exposing (..)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (css)
import ModuleExample exposing (ModuleExample)
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Pennant.V2 as Pennant
import Nri.Ui.Svg.V1 as Svg


{-| -}
example : ModuleExample msg
example =
    { name = "Nri.Ui.Pennant.V2"
    , category = Icons
    , content =
        [ Html.div [ css [ Css.displayFlex, Css.width (Css.px 200) ] ]
            [ Pennant.premiumFlag
                |> Svg.withHeight (Css.px 60)
                |> Svg.toHtml
            , Pennant.disabledPremiumFlag
                |> Svg.withHeight (Css.px 60)
                |> Svg.toHtml
            ]
        ]
    }
