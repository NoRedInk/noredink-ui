module Nri.Ui.Svg.V1 exposing
    ( Svg
    , withColor, withLabel, withWidth, withHeight
    , fromHtml, toHtml
    )

{-|

@docs Svg
@docs withColor, withLabel, withWidth, withHeight
@docs fromHtml, toHtml

-}

import Accessibility.Styled.Widget as Widget
import Css exposing (Color)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes


{-| Opaque type describing a non-interactable Html element.
-}
type Svg
    = Svg
        { icon : Html Never
        , color : Maybe Color
        , width : Maybe Css.Px
        , height : Maybe Css.Px
        , label : Maybe String
        }


{-| Tag html as being an svg.
-}
fromHtml : Html Never -> Svg
fromHtml icon =
    Svg
        { icon = icon
        , color = Nothing
        , height = Nothing
        , width = Nothing
        , label = Nothing
        }


{-| -}
withColor : Color -> Svg -> Svg
withColor color (Svg record) =
    Svg { record | color = Just color }


{-| -}
withLabel : String -> Svg -> Svg
withLabel label (Svg record) =
    Svg { record | label = Just label }


{-| -}
withWidth : Css.Px -> Svg -> Svg
withWidth width (Svg record) =
    Svg { record | width = Just width }


{-| -}
withHeight : Css.Px -> Svg -> Svg
withHeight height (Svg record) =
    Svg { record | height = Just height }


{-| Render an svg.
-}
toHtml : Svg -> Html msg
toHtml (Svg record) =
    let
        css =
            List.filterMap identity
                [ Maybe.map Css.color record.color
                , Maybe.map Css.width record.width
                , Maybe.map Css.height record.height
                ]
    in
    case ( css, record.label ) of
        ( x :: xs, Just label ) ->
            Html.div
                [ Attributes.css
                    (css ++ [ Css.display Css.inlineBlock ])
                , Widget.label label
                ]
                [ Html.map never record.icon
                ]

        ( x :: xs, Nothing ) ->
            Html.div
                [ Attributes.css
                    (css ++ [ Css.display Css.inlineBlock ])
                ]
                [ Html.map never record.icon
                ]

        ( [], Just label ) ->
            Html.div
                [ Widget.label label
                , Attributes.css [ Css.display Css.inlineBlock ]
                ]
                [ Html.map never record.icon
                ]

        ( [], Nothing ) ->
            Html.map never record.icon
