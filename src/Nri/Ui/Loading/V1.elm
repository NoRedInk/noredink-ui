module Nri.Ui.Loading.V1 exposing
    ( fadeInPage, page
    , spinningPencil, spinningDots
    )

{-| Loading behaviors

@docs fadeInPage, page
@docs spinningPencil, spinningDots

-}

import Css
import Css.Animations
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.MediaQuery.V1 as MediaQuery
import Nri.Ui.Svg.V1
import Nri.Ui.UiIcon.V1 as UiIcon
import Svg.Styled as Svg
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
        [ Nri.Ui.Svg.V1.toHtml spinningPencil
        ]


{-| -}
spinningPencil : Nri.Ui.Svg.V1.Svg
spinningPencil =
    UiIcon.edit
        |> Nri.Ui.Svg.V1.withLabel "Loading..."
        |> Nri.Ui.Svg.V1.withColor Colors.navy
        |> Nri.Ui.Svg.V1.withWidth (Css.px 100)
        |> Nri.Ui.Svg.V1.withHeight (Css.px 100)
        |> Nri.Ui.Svg.V1.withCss circlingCss


{-| -}
spinningDots : Nri.Ui.Svg.V1.Svg
spinningDots =
    Nri.Ui.Svg.V1.init "0 0 12.54 12.54"
        [ Svg.circle [ SvgAttributes.fill "#004e95", SvgAttributes.cx "6.13", SvgAttributes.cy "0.98", SvgAttributes.r "0.98" ] []
        , Svg.circle [ SvgAttributes.fill "#004cc9", SvgAttributes.cx "9.95", SvgAttributes.cy "2.47", SvgAttributes.r "0.98", SvgAttributes.transform "translate(1.12 7.67) rotate(-44.43)" ] []
        , Svg.circle [ SvgAttributes.fill "#146aff", SvgAttributes.cx "11.56", SvgAttributes.cy "6.24", SvgAttributes.r "0.98", SvgAttributes.transform "translate(5.09 17.67) rotate(-88.86)" ] []
        , Svg.circle [ SvgAttributes.fill "#0af", SvgAttributes.cx "10", SvgAttributes.cy "10.02", SvgAttributes.r "0.98", SvgAttributes.transform "translate(-4.15 9.58) rotate(-43.29)" ] []
        , Svg.circle [ SvgAttributes.fill "#d4f0ff", SvgAttributes.cx "6.2", SvgAttributes.cy "11.56", SvgAttributes.r "0.98", SvgAttributes.transform "translate(-5.6 17.29) rotate(-87.71)" ] []
        , Svg.circle [ SvgAttributes.fill "#eef9ff", SvgAttributes.cx "2.44", SvgAttributes.cy "9.92", SvgAttributes.r "0.98", SvgAttributes.transform "translate(-6.03 4.21) rotate(-42.14)" ] []
        , Svg.circle [ SvgAttributes.fill "#f5f5f5", SvgAttributes.cx "0.98", SvgAttributes.cy "6.1", SvgAttributes.r "0.98", SvgAttributes.transform "translate(-5.16 6.71) rotate(-86.57)" ] []
        , Svg.circle [ SvgAttributes.fill "#fff", SvgAttributes.cx "2.69", SvgAttributes.cy "2.37", SvgAttributes.r "0.98", SvgAttributes.transform "translate(-0.9 2.35) rotate(-41)" ] []
        ]
        |> Nri.Ui.Svg.V1.withWidth (Css.px 100)
        |> Nri.Ui.Svg.V1.withHeight (Css.px 100)
        |> Nri.Ui.Svg.V1.withCss circlingCss


circlingCss : List Css.Style
circlingCss =
    [ MediaQuery.anyMotion
        [ Css.property "animation-duration" "1s"
        , Css.property "animation-iteration-count" "infinite"
        , Css.animationName rotateKeyframes
        , Css.property "animation-timing-function" "linear"
        ]
    ]


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
