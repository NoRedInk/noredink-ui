module Nri.Ui.HighlightIcon.V1 exposing (highlighter, eraser)

{-|

@docs highlighter, eraser

    import Html.Styled exposing (Html)
    import Nri.Ui.Colors.V1 as Colors
    import Nri.Ui.HighlightIcon.V1 as HighlightIcon
    import Nri.Ui.Svg.V1 as Svg

    view : Html msg
    view =
        HighlightIcon.highlighter
            |> Svg.withColor Colors.highlightPink
            |> Svg.toHtml

-}

import Nri.Ui.Svg.V1
import Svg.Styled as Svg
import Svg.Styled.Attributes as Attributes


{-| -}
highlighter : Nri.Ui.Svg.V1.Svg
highlighter =
    Svg.svg
        [ Attributes.width "100%"
        , Attributes.height "100%"
        , Attributes.fill "currentcolor"
        , Attributes.viewBox "0 0 25 25"
        ]
        []
        |> Nri.Ui.Svg.V1.fromHtml


{-| -}
eraser : Nri.Ui.Svg.V1.Svg
eraser =
    Svg.svg
        [ Attributes.width "100%"
        , Attributes.height "100%"
        , Attributes.fill "currentcolor"
        , Attributes.viewBox "0 0 25 25"
        ]
        []
        |> Nri.Ui.Svg.V1.fromHtml
