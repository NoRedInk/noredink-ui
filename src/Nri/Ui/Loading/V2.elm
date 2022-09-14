module Nri.Ui.Loading.V2 exposing
    ( fadeInPage, page
    , spinningPencil
    )

{-| Loading behaviors

@docs fadeInPage, page
@docs spinningPencil

-}

import Css
import Css.Animations
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes
import Nri.Ui.Colors.V1 as Colors
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


circlingCss : List Css.Style
circlingCss =
    [ Css.property "animation-duration" "1s"
    , Css.property "animation-iteration-count" "infinite"
    , Css.animationName rotateKeyframes
    , Css.property "animation-timing-function" "linear"
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
