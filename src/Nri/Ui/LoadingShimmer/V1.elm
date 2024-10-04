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
        (case settings.kind of
            Line ->
                [ viewLine settings ]

            Paragraph ->
                viewParagraph settings
        )


viewLine : Settings msg -> Html msg
viewLine settings =
    Html.Styled.div [ Attributes.css [ displayFlex, property "gap" "8px", height (Css.px 16) ] ]
        [ Html.Styled.div
            [ Attributes.css
                [ flex (Css.num 1)
                , minWidth (Css.ch 1)
                , backgroundColor gray92
                , borderRadius (px 4)
                , pulseAnimation "0s"
                ]
            ]
            []
        , Html.Styled.div
            [ Attributes.css
                [ flex (Css.num 3)
                , minWidth (Css.ch 12)
                , backgroundColor gray92
                , borderRadius (px 4)
                , pulseAnimation "0.5s"
                ]
            ]
            []
        , Html.Styled.div
            [ Attributes.css
                [ flex (Css.num 2)
                , minWidth (Css.ch 6)
                , backgroundColor gray92
                , borderRadius (px 4)
                , pulseAnimation "1s"
                ]
            ]
            []
        ]


viewParagraph : Settings msg -> List (Html msg)
viewParagraph settings =
    [ viewLine settings ]


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
        [ ( 40, [ Css.Animations.opacity (Css.num 0.75) ] )
        , ( 60, [ Css.Animations.opacity (Css.num 0.75) ] )
        , ( 100, [ Css.Animations.opacity (Css.num 0) ] )
        ]
