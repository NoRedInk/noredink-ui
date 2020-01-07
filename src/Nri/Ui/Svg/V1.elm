module Nri.Ui.Svg.V1 exposing
    ( Svg
    , withColor, withLabel, withWidth, withHeight
    , fromHtml
    , toHtml, toHtmlAsButton, toHtmlAsLink
    )

{-|

@docs Svg
@docs withColor, withLabel, withWidth, withHeight
@docs fromHtml
@docs toHtml, toHtmlAsButton, toHtmlAsLink

-}

import Accessibility.Styled.Widget as Widget
import Css exposing (Color)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes
import Html.Styled.Events as Events


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


{-| Add a string aria-label property to the element.

See [Using the aria-label attribute](https://developer.mozilla.org/en-US/docs/Web/Accessibility/ARIA/ARIA_Techniques/Using_the_aria-label_attribute) for
guidelines of when and how to use this attribute.

-}
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

        attributes =
            List.filterMap identity
                [ if List.isEmpty css then
                    Nothing

                  else
                    Just (Attributes.css (Css.display Css.inlineBlock :: css))
                , Maybe.map Widget.label record.label
                ]
    in
    case attributes of
        [] ->
            Html.map never record.icon

        _ ->
            Html.div attributes [ Html.map never record.icon ]


{-| -}
toHtmlAsButton : msg -> Svg -> Html msg
toHtmlAsButton msg svg =
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
        [ toHtml svg ]


{-| -}
toHtmlAsLink : String -> Svg -> Html msg
toHtmlAsLink url svg =
    Html.a [ Attributes.href url ] [ toHtml svg ]
