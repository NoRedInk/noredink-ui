module Nri.Ui.ClickableSvg.V1 exposing (button, link)

{-|

@docs button, link

-}

import Css
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes
import Html.Styled.Events as Events
import Nri.Ui.Svg.V1 as Svg exposing (Svg)


{-| -}
button : msg -> Svg -> Html msg
button msg svg =
    Html.button
        [ Attributes.css
            [ Css.margin Css.zero
            , Css.padding Css.zero
            , Css.borderWidth Css.zero
            , Css.backgroundColor Css.transparent
            , Css.cursor Css.pointer
            ]
        , Events.onClick msg
        ]
        [ Svg.toHtml svg
        ]


{-| -}
link : String -> Svg -> Html msg
link url svg =
    Html.a
        [ Attributes.href url
        ]
        [ Svg.toHtml svg
        ]
