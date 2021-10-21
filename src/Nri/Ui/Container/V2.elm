module Nri.Ui.Container.V2 exposing
    ( view, Attribute
    , gray, default, disabled, invalid, pillow, buttony
    , plaintext, markdown, html
    , paddingPx, fullHeight, css
    )

{-| Common NoRedInk Containers

@docs view, Attribute
@docs gray, default, disabled, invalid, pillow, buttony
@docs plaintext, markdown, html
@docs Attribute, paddingPx, fullHeight, css

-}

import Css exposing (..)
import Css.Media exposing (withMedia)
import Html.Styled exposing (..)
import Html.Styled.Attributes
import Markdown
import Nri.Ui
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.MediaQuery.V1 exposing (mobile)
import Nri.Ui.Text.V5 as Text


{-| -}
type Attribute msg
    = Attribute (Settings msg -> Settings msg)


{-| PRIVATE
-}
type alias Settings msg =
    { containerType : String
    , padding : Float
    , fullHeight : Bool
    , css : List Css.Style
    , content : List (Html msg)
    }


{-| Changes the padding inside the container border around the content.
-}
paddingPx : Float -> Attribute msg
paddingPx padding =
    Attribute <| \config -> { config | padding = padding }


{-| Makes the container occupy the full height of the parent.
-}
fullHeight : Attribute msg
fullHeight =
    Attribute <| \config -> { config | fullHeight = True }


{-| -}
css : List Css.Style -> Attribute msg
css css_ =
    Attribute <| \config -> { config | css = config.css ++ css_ }


{-| -}
view : List (Attribute msg) -> Html msg
view attributes =
    let
        settings : Settings msg
        settings =
            List.foldl (\(Attribute set) -> set)
                defaultSettings
                attributes
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
        settings.content


{-| Used for the default container case.
-}
default : Attribute msg
default =
    Attribute identity


defaultSettings : Settings msg
defaultSettings =
    { containerType = "default-container"
    , padding = 20
    , fullHeight = False
    , css = defaultStyles
    , content = []
    }


defaultStyles : List Css.Style
defaultStyles =
    [ borderRadius (px 8)
    , border3 (px 1) solid Colors.gray92
    , boxShadow5 zero (px 1) (px 1) zero (rgba 0 0 0 0.25)
    , backgroundColor Colors.white
    ]


{-| Used when there are a lot of containers.
-}
gray : Attribute msg
gray =
    Attribute <|
        \config ->
            { config
                | containerType = "gray-container"
                , padding = 20
                , css = grayStyles
            }


grayStyles : List Css.Style
grayStyles =
    [ borderRadius (px 8)
    , backgroundColor Colors.gray96
    ]


{-| -}
disabled : Attribute msg
disabled =
    Attribute <|
        \config ->
            { config
                | containerType = "disabled-container"
                , padding = 20
                , css = disabledStyles
            }


disabledStyles : List Css.Style
disabledStyles =
    [ borderRadius (px 8)
    , border3 (px 1) solid Colors.gray92
    , backgroundColor Colors.white
    , color Colors.gray45
    ]


{-| -}
invalid : Attribute msg
invalid =
    Attribute <|
        \config ->
            { config
                | containerType = "invalid-container"
                , padding = 20
                , css = invalidStyles
            }


invalidStyles : List Css.Style
invalidStyles =
    [ borderRadius (px 8)
    , border3 (px 1) solid Colors.purpleLight
    , boxShadow5 zero (px 1) (px 1) zero Colors.purple
    , backgroundColor Colors.purpleLight
    ]


{-| Used for containers of interactive elements.
-}
pillow : Attribute msg
pillow =
    Attribute <|
        \config ->
            { config
                | containerType = "pillow-container"
                , padding = 40
                , css = pillowStyles
            }


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
buttony : Attribute msg
buttony =
    Attribute <|
        \config ->
            { config
                | containerType = "buttony-container"
                , css = buttonyStyles
            }


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


{-| Provide a list of custom HTML.
-}
html : List (Html msg) -> Attribute msg
html content =
    Attribute <| \config -> { config | content = content }


{-| Provide a plain-text string.
-}
plaintext : String -> Attribute msg
plaintext content =
    Attribute <| \config -> { config | content = [ text content ] }


{-| Provide a string that will be rendered as markdown.
-}
markdown : String -> Attribute msg
markdown content =
    Attribute <|
        \config ->
            { config
                | content =
                    Markdown.toHtml Nothing content
                        |> List.map fromUnstyled
            }
