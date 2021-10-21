module Nri.Ui.Container.V2 exposing
    ( gray, default, disabled, invalid, pillow, buttony
    , Attribute, paddingPx, fullHeight, css
    )

{-| Common NoRedInk Containers

@docs gray, default, disabled, invalid, pillow, buttony
@docs Attribute, paddingPx, fullHeight, css

-}

import Css exposing (..)
import Css.Media exposing (withMedia)
import Html.Styled exposing (..)
import Html.Styled.Attributes
import Nri.Ui
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.MediaQuery.V1 exposing (mobile)
import Nri.Ui.Text.V5 as Text


{-| -}
type Attribute
    = CustomPadding Float
    | FullHeight
    | Css (List Css.Style)


{-| Changes the padding inside the container border around the content.
-}
paddingPx : Float -> Attribute
paddingPx =
    CustomPadding


{-| Makes the container occupy the full height of the parent.
-}
fullHeight : Attribute
fullHeight =
    FullHeight


{-| -}
css : List Css.Style -> Attribute
css =
    Css


{-| PRIVATE
-}
type alias Settings =
    { containerType : String
    , padding : Float
    , fullHeight : Bool
    , css : List Css.Style
    }


{-| PRIVATE
-}
applyAttribute : Attribute -> Settings -> Settings
applyAttribute attribute styles =
    case attribute of
        CustomPadding padding ->
            { styles | padding = padding }

        FullHeight ->
            { styles | fullHeight = True }

        Css css_ ->
            { styles | css = css_ }


{-| PRIVATE
-}
renderContainer : Settings -> List Attribute -> List (Html msg) -> Html msg
renderContainer defaultSettings attributes content =
    let
        settings : Settings
        settings =
            List.foldl applyAttribute defaultSettings attributes
    in
    Nri.Ui.styled div
        settings.containerType
        ([ padding (px settings.padding)
         , if settings.fullHeight then
            height (pct 100)

           else
            batch []
         ]
            ++ settings.css
        )
        []
        content


{-| Used for the default container case.
-}
default : List Attribute -> Html msg -> Html msg
default attributes content =
    renderContainer
        { containerType = "default-container"
        , padding = 20
        , fullHeight = False
        , css =
            [ borderRadius (px 8)
            , border3 (px 1) solid Colors.gray92
            , boxShadow5 zero (px 1) (px 1) zero (rgba 0 0 0 0.25)
            , backgroundColor Colors.white
            ]
        }
        attributes
        [ content ]


{-| Used when there are a lot of containers.
-}
gray : List Attribute -> Html msg -> Html msg
gray attributes content =
    renderContainer
        { containerType = "gray-container"
        , padding = 20
        , fullHeight = False
        , css =
            [ borderRadius (px 8)
            , backgroundColor Colors.gray96
            ]
        }
        attributes
        [ content ]


{-| -}
disabled : List Attribute -> Html msg -> Html msg
disabled attributes content =
    renderContainer
        { containerType = "disabled-container"
        , padding = 20
        , fullHeight = False
        , css =
            [ borderRadius (px 8)
            , border3 (px 1) solid Colors.gray92
            , backgroundColor Colors.white
            , color Colors.gray45
            ]
        }
        attributes
        [ content ]


{-| -}
invalid : List Attribute -> Html msg -> Html msg
invalid attributes content =
    renderContainer
        { containerType = "invalid-container"
        , padding = 20
        , fullHeight = False
        , css =
            [ borderRadius (px 8)
            , border3 (px 1) solid Colors.purpleLight
            , boxShadow5 zero (px 1) (px 1) zero Colors.purple
            , backgroundColor Colors.purpleLight
            ]
        }
        attributes
        [ content ]


{-| Used for containers of interactive elements.
-}
pillow : List Attribute -> Html msg -> Html msg
pillow attributes content =
    renderContainer
        { containerType = "pillow-container"
        , padding = 40
        , fullHeight = False
        , css = pillowStyles
        }
        attributes
        [ content ]


pillowStyles : List Style
pillowStyles =
    [ borderRadius (px 20)
    , border3 (px 1) solid Colors.gray92
    , boxShadow5 zero (px 2) (px 4) zero (rgba 0 0 0 0.25)
    , backgroundColor Colors.white
    , withMedia [ mobile ]
        [ borderRadius (px 8)
        , padding (px 20)
        ]
    ]


{-| Used for clickable cards
-}
buttony : List Attribute -> Html msg -> Html msg
buttony attributes content =
    renderContainer
        { containerType = "buttony-container"
        , padding = 20
        , fullHeight = False
        , css = buttonyStyles
        }
        attributes
        [ content ]


buttonyStyles : List Style
buttonyStyles =
    [ borderRadius (px 20)
    , border3 (px 1) solid Colors.gray85
    , borderBottom3 (px 4) solid Colors.gray85
    , backgroundColor Colors.white
    , withMedia [ mobile ]
        [ borderRadius (px 8)
        ]
    ]
