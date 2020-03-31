module Examples.Pennant exposing (example)

{-|

@docs example

-}

import Category exposing (Category(..))
import Css exposing (..)
import Example exposing (Example)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (css)
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Pennant.V2 as Pennant
import Nri.Ui.Svg.V1 as Svg


{-| -}
example : Example () ()
example =
    { name = "Nri.Ui.Pennant.V2"
    , categories = List.singleton Icons
    , state = ()
    , update = \_ state -> ( state, Cmd.none )
    , subscriptions = \_ -> Sub.none
    , view =
        \_ ->
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
