module Nri.Ui.Svg.V1 exposing
    ( Svg
    , withColor, withLabel, withWidth, withHeight, withCss, withNriDescription
    , fromHtml, toHtml
    , toRawSvg
    )

{-|

@docs Svg
@docs withColor, withLabel, withWidth, withHeight, withCss, withNriDescription
@docs fromHtml, toHtml
@docs toRawSvg

-}

import Accessibility.Styled.Aria as Aria
import Accessibility.Styled.Role as Role
import Css exposing (Color)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes
import Nri.Ui.Html.Attributes.V2 as AttributesExtra
import Svg.Styled as Svg


{-| Opaque type describing a non-interactable Html element.
-}
type Svg
    = Svg
        { icon : Html Never
        , color : Maybe Color
        , width : Maybe Css.Px
        , height : Maybe Css.Px
        , css : List Css.Style
        , label : Maybe String
        , attributes : List (Html.Attribute Never)
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
        , css = []
        , label = Nothing
        , attributes = []
        }


{-| -}
withColor : Color -> Svg -> Svg
withColor color (Svg record) =
    Svg { record | color = Just color }


{-| Add a string aria-label property to the element.

See [Using the aria-label attribute](https://developer.mozilla.org/en-US/docs/Web/Accessibility/ARIA/ARIA_Techniques/Using_the_aria-label_attribute) for
guidelines of when and how to use this attribute.

Note that when the label is _not_ present, `aria-hidden` will be added. See <https://css-tricks.com/accessible-svg-icons/> for a quick summary on why.

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


{-| Css for the SVG's container.
-}
withCss : List Css.Style -> Svg -> Svg
withCss css (Svg record) =
    Svg { record | css = record.css ++ css }


{-| -}
withCustom : List (Html.Attribute Never) -> Svg -> Svg
withCustom attributes (Svg record) =
    Svg { record | attributes = attributes ++ record.attributes }


{-| -}
withNriDescription : String -> Svg -> Svg
withNriDescription description =
    withCustom [ AttributesExtra.nriDescription description ]


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
                ++ record.css

        attributes =
            List.filterMap identity
                [ if List.isEmpty css then
                    Nothing

                  else
                    Just (Attributes.css (Css.display Css.inlineBlock :: css))
                , Maybe.map Aria.label record.label
                    |> Maybe.withDefault (Aria.hidden True)
                    |> Just
                , Just Role.img
                ]
                ++ record.attributes
    in
    Html.map never (Html.div attributes [ record.icon ])


{-| Extract an svg, dropping any attributes passed through.
-}
toRawSvg : Svg -> Svg.Svg msg
toRawSvg (Svg record) =
    Html.map never record.icon
