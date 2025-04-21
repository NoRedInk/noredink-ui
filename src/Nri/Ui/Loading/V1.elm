module Nri.Ui.Loading.V1 exposing
    ( fadeInPage, page
    , spinning
    , spinningPencil, spinningDots
    )

{-| Loading behaviors

@docs fadeInPage, page
@docs spinning
@docs spinningPencil, spinningDots

-}

import Css
import Css.Animations
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes
import List.Extra
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.MediaQuery.V1 as MediaQuery
import Nri.Ui.Svg.V1 as Svg exposing (Svg)
import Nri.Ui.UiIcon.V2 as UiIcon
import Svg.Styled
import Svg.Styled.Attributes as SvgAttributes


{-| View a full-screen loading page that fades into view.
-}
fadeInPage : Html msg
fadeInPage =
    loading_
        [ Css.property "animation-delay" "1s"
        , Css.property "animation-duration" "1.5s"
        , Css.property "animation-fill-mode" "forwards"
        , Css.animationName fadeInKeyframes
        , Css.property "animation-timing-function" "linear"
        , Css.opacity Css.zero
        ]


{-| View a full-screen loading page.
-}
page : Html msg
page =
    loading_ []


loading_ : List Css.Style -> Html msg
loading_ withCss =
    Html.div
        [ Attributes.css
            ([ Css.backgroundColor Colors.white
             , Css.position Css.fixed
             , Css.displayFlex
             , Css.alignItems Css.center
             , Css.justifyContent Css.center
             , Css.width (Css.vw 100)
             , Css.height (Css.vh 100)
             , Css.top Css.zero
             , Css.left Css.zero
             , Css.zIndex (Css.int 10000)
             ]
                ++ withCss
            )
        ]
        [ Svg.toHtml spinningPencil
        ]


{-| -}
spinning : Css.Px -> Css.Color -> Html msg
spinning size color =
    Html.div
        []
        [ spinningDots
            |> Svg.withCss [ MediaQuery.anyMotion [ Css.display Css.none ] ]
            |> Svg.withWidth size
            |> Svg.withHeight size
            |> Svg.toHtml
        , spinningPencil
            |> Svg.withColor color
            |> Svg.withCss [ MediaQuery.prefersReducedMotion [ Css.display Css.none ] ]
            |> Svg.withWidth size
            |> Svg.withHeight size
            |> Svg.toHtml
        ]


{-| -}
spinningPencil : Svg
spinningPencil =
    UiIcon.edit
        |> Svg.withLabel "Loading..."
        |> Svg.withColor Colors.navy
        |> Svg.withWidth (Css.px 140)
        |> Svg.withHeight (Css.px 140)
        |> Svg.withViewBox "-20 -20 140 140"
        |> Svg.withCss circlingCss


{-| -}
spinningDots : Svg
spinningDots =
    let
        dotColors =
            [ "#fff"
            , "#f5f5f5"
            , "#eef9ff"
            , "#d4f0ff"
            , "#0af"
            , "#146aff"
            , "#004cc9"
            , "#004e95"
            ]

        rotatedColors rotateWith =
            let
                ( before, after ) =
                    List.Extra.splitAt rotateWith dotColors
            in
            after ++ before

        circle index attributes =
            let
                colors =
                    rotatedColors index
            in
            Svg.Styled.circle
                (List.filterMap identity
                    [ Maybe.map SvgAttributes.fill (List.head colors)
                    , Just (SvgAttributes.css (colorChangeCss (List.reverse colors)))
                    ]
                    ++ attributes
                )
                []
    in
    [ [ SvgAttributes.cx "6.13", SvgAttributes.cy "0.98", SvgAttributes.r "0.98" ]
    , [ SvgAttributes.cx "9.95", SvgAttributes.cy "2.47", SvgAttributes.r "0.98", SvgAttributes.transform "translate(1.12 7.67) rotate(-44.43)" ]
    , [ SvgAttributes.cx "11.56", SvgAttributes.cy "6.24", SvgAttributes.r "0.98", SvgAttributes.transform "translate(5.09 17.67) rotate(-88.86)" ]
    , [ SvgAttributes.cx "10", SvgAttributes.cy "10.02", SvgAttributes.r "0.98", SvgAttributes.transform "translate(-4.15 9.58) rotate(-43.29)" ]
    , [ SvgAttributes.cx "6.2", SvgAttributes.cy "11.56", SvgAttributes.r "0.98", SvgAttributes.transform "translate(-5.6 17.29) rotate(-87.71)" ]
    , [ SvgAttributes.cx "2.44", SvgAttributes.cy "9.92", SvgAttributes.r "0.98", SvgAttributes.transform "translate(-6.03 4.21) rotate(-42.14)" ]
    , [ SvgAttributes.cx "0.98", SvgAttributes.cy "6.1", SvgAttributes.r "0.98", SvgAttributes.transform "translate(-5.16 6.71) rotate(-86.57)" ]
    , [ SvgAttributes.cx "2.69", SvgAttributes.cy "2.37", SvgAttributes.r "0.98", SvgAttributes.transform "translate(-0.9 2.35) rotate(-41)" ]
    ]
        |> List.indexedMap circle
        |> Svg.init "0 0 12.54 12.54"
        |> Svg.withWidth (Css.px 140)
        |> Svg.withHeight (Css.px 140)


circlingCss : List Css.Style
circlingCss =
    [ MediaQuery.anyMotion
        [ Css.property "animation-duration" "1s"
        , Css.property "animation-iteration-count" "infinite"
        , Css.animationName rotateKeyframes
        , Css.property "animation-timing-function" "linear"
        ]
    ]


colorChangeCss : List String -> List Css.Style
colorChangeCss colors =
    [ Css.property "animation-duration" "2s"
    , Css.property "animation-iteration-count" "infinite"
    , Css.animationName (colorChangeKeyFrames colors)
    , Css.property "animation-timing-function" "linear"
    ]


colorChangeKeyFrames : List String -> Css.Animations.Keyframes {}
colorChangeKeyFrames colors =
    let
        colorCount =
            List.length colors
    in
    colors
        |> List.indexedMap
            (\index color ->
                ( round (100 * (toFloat index + 1) / toFloat colorCount)
                , [ Css.Animations.property "fill" color ]
                )
            )
        |> Css.Animations.keyframes


rotateKeyframes : Css.Animations.Keyframes {}
rotateKeyframes =
    Css.Animations.keyframes
        [ ( 0, [ Css.Animations.transform [ Css.rotate (Css.deg -360) ] ] )
        ]


fadeInKeyframes : Css.Animations.Keyframes {}
fadeInKeyframes =
    Css.Animations.keyframes
        [ ( 0, [ Css.Animations.opacity Css.zero ] )
        , ( 100, [ Css.Animations.opacity (Css.num 1) ] )
        ]
