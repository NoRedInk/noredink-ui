module Nri.Ui.LoadingShimmer.V1 exposing
    ( Attribute
    , css
    , custom
    , fillContainerWidth
    , id
    , line
    , nriDescription
    , paragraph
    , testId
    , view
    )

import Accessibility.Styled as Html exposing (Attribute, Html)
import Css exposing (..)
import Css.Animations
import Html.Styled
import Html.Styled.Attributes as Attributes
import Nri.Ui
import Nri.Ui.Colors.V1 exposing (..)
import Nri.Ui.Html.Attributes.V2 as ExtraAttributes


type LoadingShimmerKind
    = Line
    | Paragraph


type LoadingShimmerWidth
    = WidthExact Int
    | WidthFillContainer
    | WidthBounded { min : Int, max : Int }


{-| -}
type Attribute msg
    = Attribute (Settings msg -> Settings msg)


type alias Settings msg =
    { styles : List Css.Style
    , customAttributes : List (Html.Styled.Attribute msg)
    , width : LoadingShimmerWidth
    , kind : LoadingShimmerKind
    }


defaultSettings : Settings msg
defaultSettings =
    { styles = []
    , customAttributes = []
    , width = WidthFillContainer
    , kind = Line
    }


buildSettings : List (Attribute msg) -> Settings msg
buildSettings =
    List.foldl (\(Attribute f) acc -> f acc) defaultSettings


{-| Use this helper to add custom attributes.

Do NOT use this helper to add css styles, as they may not be applied the way
you want/expect if underlying styles change.
Instead, please use the `css` helper.

-}
custom : List (Html.Attribute msg) -> Attribute msg
custom attributes =
    Attribute <|
        \config ->
            { config
                | customAttributes = List.append config.customAttributes attributes
            }


{-| -}
nriDescription : String -> Attribute msg
nriDescription description =
    custom [ ExtraAttributes.nriDescription description ]


{-| -}
testId : String -> Attribute msg
testId id_ =
    custom [ ExtraAttributes.testId id_ ]


{-| -}
id : String -> Attribute msg
id id_ =
    custom [ Attributes.id id_ ]


{-| Add some custom CSS to the text. If you find yourself using this a lot,
please add a stricter attribute to noredink-ui!
-}
css : List Style -> Attribute msg
css styles =
    Attribute (\config -> { config | styles = config.styles ++ styles })


type LoadingShimmer msg
    = LoadingShimmer (Settings msg)


set :
    (Settings msg -> Settings msg)
    -> Attribute msg
set with =
    Attribute (\config -> with config)


setWidth : LoadingShimmerWidth -> Attribute msg
setWidth w =
    set (\attributes -> { attributes | width = w })


{-| Leave the maxiumum width unbounded (there is a minimum width).
-}
fillContainerWidth : Attribute msg
fillContainerWidth =
    setWidth WidthFillContainer


{-| -}
view : List (Attribute msg) -> Html msg
view attributes =
    let
        settings : Settings msg
        settings =
            buildSettings attributes
    in
    Html.Styled.div
        (Attributes.css
            [ batch settings.styles
            ]
            :: settings.customAttributes
        )
        [ case settings.kind of
            Line ->
                viewLine settings

            Paragraph ->
                viewParagraph settings
        ]


backgroundImageGradient : BackgroundImage (ListStyle {})
backgroundImageGradient =
    let
        transparentMixableStop =
            stop (rgba 0 0 0 0)

        darkMixableStop =
            stop <| rgba 0 0 0 0.1
    in
    linearGradient2
        toRight
        transparentMixableStop
        darkMixableStop
        [ transparentMixableStop ]


viewLine : Settings msg -> Html msg
viewLine settings =
    let
        textSkeletonColor =
            gray85
    in
    Html.Styled.div
        [ Attributes.css
            [ height (Css.px 16)
            , position relative
            , overflow hidden
            , minWidth (Css.ch 1)
            , case settings.width of
                WidthExact w ->
                    batch [ width (Css.px <| toFloat w) ]

                WidthFillContainer ->
                    batch [ width (pct 100) ]

                WidthBounded { min, max } ->
                    batch [ minWidth (px <| toFloat min), maxWidth (px <| toFloat max) ]
            , borderRadius (px 4)
            , backgroundColor textSkeletonColor
            , before
                [ position absolute
                , top zero
                , bottom zero
                , left zero
                , right zero
                , backgroundImage backgroundImageGradient
                , shimmerAnimation
                , Css.property "content" "\" \""
                ]
            ]
        ]
        []


viewParagraph : Settings msg -> Html msg
viewParagraph settings =
    let
        textSkeletonColor =
            gray85
    in
    Html.Styled.div
        [ Attributes.css
            [ property "display" "grid"
            , property "gap" "8px"
            , property "grid-template-columns" "repeat(12, 1fr)"
            , height (Css.px 24)
            , padding (px 4)
            , position relative
            , overflow hidden
            , before
                [ position absolute
                , top zero
                , bottom zero
                , left zero
                , right zero
                , backgroundImage backgroundImageGradient
                , shimmerAnimation
                , Css.property "content" "\" \""
                ]
            ]
        ]
        [ Html.Styled.div
            [ Attributes.css
                [ minWidth (Css.ch 1)
                , borderRadius (px 4)
                , property "grid-column" "span 2"
                , backgroundColor textSkeletonColor
                ]
            ]
            []
        , Html.Styled.div
            [ Attributes.css
                [ minWidth (Css.ch 12)
                , borderRadius (px 4)
                , property "grid-column" "span 6"
                , backgroundColor textSkeletonColor
                ]
            ]
            []
        , Html.Styled.div
            [ Attributes.css
                [ minWidth (Css.ch 6)
                , borderRadius (px 4)
                , property "grid-column" "span 4"
                , backgroundColor textSkeletonColor
                ]
            ]
            []
        ]


line : Attribute msg
line =
    set
        (\attributes ->
            { attributes | kind = Line }
        )


paragraph : Attribute msg
paragraph =
    set
        (\attributes ->
            { attributes | kind = Paragraph }
        )


pulseAnimation : String -> Style
pulseAnimation delay =
    batch
        [ animationName pulse
        , property "animation-duration" "3s"
        , property "animation-timing-function" "cubic-bezier(0.83, 0, 0.17, 1)"
        , property "animation-iteration-count" "infinite"
        , property "animation-delay" delay
        , property "animation-fill-mode" "forwards"
        , opacity (Css.num 0)
        ]


pulse : Css.Animations.Keyframes {}
pulse =
    Css.Animations.keyframes
        [ ( 40, [ Css.Animations.opacity (Css.num 1) ] )
        , ( 60, [ Css.Animations.opacity (Css.num 1) ] )
        , ( 100, [ Css.Animations.opacity (Css.num 0) ] )
        ]


shimmerAnimation : Style
shimmerAnimation =
    batch
        [ property "animation-duration" "2s"
        , property "animation-iteration-count" "infinite"
        , property "animation-fill-mode" "forwards"
        , property "animation-timing-function" "cubic-bezier(0.65, 0, 0.35, 1)"
        , animationName shimmer
        , property "transform-origin" "left"
        ]


shimmer : Css.Animations.Keyframes {}
shimmer =
    Css.Animations.keyframes
        [ ( 0, [ Css.Animations.transform [ translateX (pct -100) ] ] )
        , ( 100, [ Css.Animations.transform [ translateX (pct 100) ] ] )
        ]
