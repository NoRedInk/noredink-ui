module Examples.Pennant exposing (example)

{-|

@docs example

-}

import Css exposing (..)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (css)
import ModuleExample exposing (Category(..), ModuleExample)
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Pennant.V2 as Pennant
import Nri.Ui.Svg.V1 as Svg


{-| -}
example : ModuleExample msg
example =
    { name = "Nri.Ui.Pennant.V2"
    , category = Icons
    , content =
        [ Html.div [ css [ displayFlex, alignItems center ] ]
            [ Html.span [ css [ Fonts.baseFont, Css.fontSize (Css.px 16) ] ]
                [ Html.text "A premiumFlag indicates that content is Premium"
                ]
            , Pennant.premiumFlag
                |> Svg.withWidth (Css.px 30)
                |> Svg.withHeight (Css.px 16)
                |> Svg.toHtml
            ]
        ]
    }
