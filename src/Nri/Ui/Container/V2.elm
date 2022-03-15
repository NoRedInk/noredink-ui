module Nri.Ui.Container.V2 exposing
    ( view, Attribute
    , paddingPx, custom, css, testId, id
    , plaintext, markdown, html
    , gray, default, disabled, invalid, pillow, buttony
    )

{-| Common NoRedInk Containers


# Changelog


## Changes from V1

  - removes fullHeight
  - changes the API from providing many themed functions that create HTML (`alternate`, `general`, etc.) to having a single `view` helper that takes a list of attributes (including themed attributes and content attributes)
  - adds `custom`, `testId`, `id`
  - adds `plaintext` helper and `markdown` helper
  - renames themes from:
      - `alternate` -> `gray`
      - `general` -> `default`
      - `interactable` -> `pillow`
  - removes `interactableWithLabel` theme
  - adds `buttony` theme


# Documentation


## View

@docs view, Attribute
@docs paddingPx, custom, css, testId, id


## Content

@docs plaintext, markdown, html


## Themes

@docs gray, default, disabled, invalid, pillow, buttony

-}

import Css exposing (..)
import Css.Media exposing (withMedia)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes
import Markdown
import Nri.Ui
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Html.Attributes.V2 as ExtraAttributes
import Nri.Ui.MediaQuery.V1 exposing (mobile)


{-| -}
type Attribute msg
    = Attribute (Settings msg -> Settings msg)


{-| PRIVATE
-}
type alias Settings msg =
    { containerType : String
    , padding : Float
    , css : List Css.Style
    , content : List (Html msg)
    , attributes : List (Html.Attribute msg)
    }


{-| Changes the padding inside the container border around the content.
-}
paddingPx : Float -> Attribute msg
paddingPx padding =
    Attribute <| \config -> { config | padding = padding }


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
                | attributes =
                    config.attributes ++ attributes
            }


{-| -}
testId : String -> Attribute msg
testId id_ =
    custom [ ExtraAttributes.testId id_ ]


{-| -}
id : String -> Attribute msg
id id_ =
    custom [ Html.Styled.Attributes.id id_ ]


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
        (padding (px settings.padding) :: settings.css)
        settings.attributes
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
    , css = defaultStyles
    , content = []
    , attributes = []
    }


defaultStyles : List Css.Style
defaultStyles =
    [ borderRadius (px 8)
    , border3 (px 1) solid Colors.gray92
    , property "box-shadow" "0 0.8px 0.7px hsl(0deg 0% 0% / 0.1), 0 1px 1px -1.2px hsl(0deg 0% 0% / 0.1), 0 5px 2.5px -2.5px hsl(0deg 0% 0% / 0.1);"
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
    , property "box-shadow" "0 0.5px 0.7px hsl(0deg 0% 0% / 0.075), 0 1.6px 2px -0.8px hsl(0deg 0% 0% / 0.075), 0 4.1px 5.2px -1.7px hsl(0deg 0% 0% / 0.075), 5px 10px 12.6px -2.5px hsl(0deg 0% 0% / 0.075);"
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

Note that you may need to remove extra margin added by default
to `p` tags by user agents.

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
