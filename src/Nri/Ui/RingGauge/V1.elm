module Nri.Ui.RingGauge.V1 exposing (view)

{-| A ring gauge made with svg that shows completion by stroking the circumference
with a specific color.

Uses the strategy discussed here:
<https://medium.com/@heyoka/scratch-made-svg-donut-pie-charts-in-html5-2c587e935d72>

@docs view

-}

import Css exposing (Color)
import Nri.Ui.Colors.Extra exposing (toCssString)
import Nri.Ui.Svg.V1 as Svg exposing (Svg)
import Svg.Styled exposing (circle)
import Svg.Styled.Attributes exposing (..)


{-| Shows a configurable SVG ring guage
-}
view :
    { backgroundColor : Color
    , emptyColor : Color
    , filledColor : Color
    , percentage : Float
    }
    -> Svg
view config =
    let
        -- Svgs are scalable. For convenience, we'll use 100 for circumference.
        ringCircumference =
            100

        ringRadius =
            ringCircumference / (2 * pi)

        -- NOTE: The dashArray determines how much of the circumference to paint
        -- with the strokeColor.
        dashArray =
            let
                filledArcLength =
                    ringCircumference * (config.percentage / 100)

                emptyArcLength =
                    ringCircumference * (1 - (config.percentage / 100))
            in
            String.fromFloat filledArcLength ++ " " ++ String.fromFloat emptyArcLength

        -- HACK: Safari shows a sliver of completion, even for an empty ring,
        -- this works around it by making the filledColor the same as the
        -- background (emptyColor) when the percentage is 0.
        filledColor =
            if config.percentage <= 0 then
                config.emptyColor

            else
                config.filledColor

        -- NOTE: Browsers are inconsistent about where they draw the stroke
        -- (inside the circle, on the circle, outside the circle). As a result,
        -- we draw a larger stroke than needed and put a white circle on top
        -- as the hole. It's also recommended that the container for the svg
        -- has borderRadius: 50% and overflow: hidden to ensure the circle is
        -- trimmed to fit the container.
        holeRadius =
            6.5

        centerpoint =
            "21"

        ringAttributes =
            [ cx centerpoint
            , cy centerpoint
            , r (String.fromFloat ringRadius)
            , fill "none"
            , strokeWidth "25"
            ]
    in
    Svg.init "0 0 42 42"
        [ -- Empty ring
          circle
            (stroke (toCssString config.emptyColor)
                :: ringAttributes
            )
            []
        , -- Filled ring
          circle
            (stroke (toCssString filledColor)
                :: strokeDasharray dashArray
                :: strokeDashoffset "25"
                :: ringAttributes
            )
            []
        , -- Hole
          circle
            [ cx centerpoint
            , cy centerpoint
            , r (String.fromFloat holeRadius)
            , fill (toCssString config.backgroundColor)
            ]
            []
        ]
        |> Svg.withCss [ Css.borderRadius (Css.pct 50) ]
