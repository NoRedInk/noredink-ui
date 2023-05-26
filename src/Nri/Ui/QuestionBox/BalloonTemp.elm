module Nri.Ui.QuestionBox.BalloonTemp exposing
    ( Attribute
    , containerCss
    , css
    , customTheme
    , html
    , id
    , nriDescription
    , view
    )

import Content
import Css exposing (..)
import Html.Styled as Html exposing (Html, div, styled)
import Html.Styled.Attributes as Attributes
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Html.Attributes.V2 as ExtraAttributes
import Nri.Ui.Shadows.V1 as Shadows



-- TOOLTIP VIEWS


{-| Green balloon without an arrow by default.

     __________
    |         |
    |_________|

-}
view : List (Attribute msg) -> Html msg
view customizations =
    view_ (customizationsToConfig customizations)


{-| Balloon's attributes.
-}
type Attribute msg
    = Attribute (Config msg -> Config msg)


setTheme : Theme -> Attribute msg
setTheme theme =
    Attribute (\config -> { config | theme = theme })


{-| Custom theme: set the background & text color.
-}
customTheme : { backgroundColor : Css.Color, color : Css.Color } -> Attribute msg
customTheme =
    setTheme


{-| -}
containerCss : List Style -> Attribute msg
containerCss styles =
    Attribute (\config -> { config | containerCss = List.append config.containerCss styles })


{-| -}
css : List Style -> Attribute msg
css styles =
    Attribute (\config -> { config | css = List.append config.css styles })


{-| Use this helper to add custom attributes.

Do NOT use this helper to add css styles, as they may not be applied the way you want/expect if underlying Balloon styles change.
Instead, please use the `css` helper.

-}
custom : List (Html.Attribute msg) -> Attribute msg
custom attributes =
    Attribute
        (\config ->
            { config
                | customAttributes = List.append config.customAttributes attributes
            }
        )


{-| -}
nriDescription : String -> Attribute msg
nriDescription description =
    custom [ ExtraAttributes.nriDescription description ]


{-| -}
id : String -> Attribute msg
id id_ =
    custom [ Attributes.id id_ ]


{-| Provide a list of custom HTML.
-}
html : List (Html msg) -> Attribute msg
html =
    Attribute << Content.html



-- INTERNALS


type alias Config msg =
    { theme : Theme
    , containerCss : List Css.Style
    , contentId : Maybe String
    , css : List Css.Style
    , customAttributes : List (Html.Attribute msg)
    , content : List (Html msg)
    }


{-| Default configuration
-}
defaultConfig : Config msg
defaultConfig =
    { theme = defaultGreenTheme
    , containerCss = []
    , contentId = Nothing
    , css = [ Css.padding (Css.px 20) ]
    , customAttributes = []
    , content = []
    }


type alias Theme =
    { backgroundColor : Css.Color
    , color : Css.Color
    }


defaultGreenTheme : Theme
defaultGreenTheme =
    { backgroundColor = Colors.greenDarkest
    , color = Colors.white
    }


view_ : Config msg -> Html msg
view_ config =
    container
        (Attributes.css config.containerCss :: config.customAttributes)
        [ viewBalloon config
        ]


container : List (Html.Attribute msg) -> List (Html msg) -> Html msg
container attributes =
    styled div
        []
        attributes


viewBalloon :
    { config
        | theme : Theme
        , contentId : Maybe String
        , css : List Css.Style
        , content : List (Html msg)
    }
    -> Html msg
viewBalloon config =
    styled div
        [ display inlineBlock
        , lineHeight (num 1.4)
        , textAlign left
        , position relative
        , Css.borderRadius (px borderRounding)
        , Shadows.high
        , backgroundColor config.theme.backgroundColor
        , border3 (px 1) solid config.theme.backgroundColor
        , color config.theme.color
        , Fonts.baseFont
        , fontSize (px 15)
        , Css.batch config.css
        ]
        (case config.contentId of
            Nothing ->
                []

            Just id_ ->
                [ Attributes.id id_ ]
        )
        config.content


borderRounding : Float
borderRounding =
    8


customizationsToConfig : List (Attribute msg) -> Config msg
customizationsToConfig customizations =
    List.foldl (\(Attribute f) a -> f a) defaultConfig customizations
