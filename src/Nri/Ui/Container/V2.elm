module Nri.Ui.Container.V2 exposing
    ( gray, default, disabled, invalid, pillow, buttony
    , plaintext, markdown, html
    , Attribute, paddingPx, fullHeight, css
    )

{-| Common NoRedInk Containers

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
    Attribute <| \config -> { config | css = css_ }


{-| PRIVATE
-}
renderContainer : Settings msg -> List (Attribute msg) -> Html msg
renderContainer defaultSettings attributes =
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
default : List (Attribute msg) -> Html msg
default attributes =
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
        , content = []
        }
        attributes


{-| Used when there are a lot of containers.
-}
gray : List (Attribute msg) -> Html msg
gray attributes =
    renderContainer
        { containerType = "gray-container"
        , padding = 20
        , fullHeight = False
        , css =
            [ borderRadius (px 8)
            , backgroundColor Colors.gray96
            ]
        , content = []
        }
        attributes


{-| -}
disabled : List (Attribute msg) -> Html msg
disabled attributes =
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
        , content = []
        }
        attributes


{-| -}
invalid : List (Attribute msg) -> Html msg
invalid attributes =
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
        , content = []
        }
        attributes


{-| Used for containers of interactive elements.
-}
pillow : List (Attribute msg) -> Html msg
pillow attributes =
    renderContainer
        { containerType = "pillow-container"
        , padding = 40
        , fullHeight = False
        , css = pillowStyles
        , content = []
        }
        attributes


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
buttony : List (Attribute msg) -> Html msg
buttony attributes =
    renderContainer
        { containerType = "buttony-container"
        , padding = 20
        , fullHeight = False
        , css = buttonyStyles
        , content = []
        }
        attributes


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
