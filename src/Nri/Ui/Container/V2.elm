module Nri.Ui.Container.V2 exposing
    ( view, Attribute
    , custom, testId, id
    , css, notMobileCss, mobileCss, quizEngineMobileCss
    , paddingPx
    , plaintext, markdown, html
    , gray, default, disabled, invalid, pillow, buttony
    )

{-| Common NoRedInk Containers


# TODO in next version:

  - remove `invalid`


# Changelog


## Patch changes

  - use `Shadows`
  - add notMobileCss, mobileCss, quizEngineMobileCss
  - use internal `Content` module


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
@docs custom, testId, id

@docs css, notMobileCss, mobileCss, quizEngineMobileCss
@docs paddingPx


## Content

@docs plaintext, markdown, html


## Themes

@docs gray, default, disabled, invalid, pillow, buttony

-}

import Content
import Css exposing (..)
import Css.Media exposing (withMedia)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes
import Nri.Ui
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Html.Attributes.V2 as ExtraAttributes
import Nri.Ui.MediaQuery.V1 as MediaQuery
import Nri.Ui.Shadows.V1 as Shadows


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


{-| Set styles that will only apply if the viewport is wider than NRI's mobile breakpoint.

Equivalent to:

    Container.css
        [ Css.Media.withMedia [ Nri.Ui.MediaQuery.V1.notMobile ] styles ]

-}
notMobileCss : List Style -> Attribute msg
notMobileCss styles =
    css [ Css.Media.withMedia [ MediaQuery.notMobile ] styles ]


{-| Set styles that will only apply if the viewport is narrower than NRI's mobile breakpoint.

Equivalent to:

    Container.css
        [ Css.Media.withMedia [ Nri.Ui.MediaQuery.V1.mobile ] styles ]

-}
mobileCss : List Style -> Attribute msg
mobileCss styles =
    css [ Css.Media.withMedia [ MediaQuery.mobile ] styles ]


{-| Set styles that will only apply if the viewport is narrower than NRI's quiz-engine-specific mobile breakpoint.

Equivalent to:

    Container.css
        [ Css.Media.withMedia [ Nri.Ui.MediaQuery.V1.quizEngineMobile ] styles ]

-}
quizEngineMobileCss : List Style -> Attribute msg
quizEngineMobileCss styles =
    css [ Css.Media.withMedia [ MediaQuery.quizEngineMobile ] styles ]


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
    , Shadows.low
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


{-| DEPRECATED -- this will be removed in the next version of this component.
-}
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
    , Shadows.medium
    , backgroundColor Colors.white
    , withMedia [ MediaQuery.mobile ]
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
    , withMedia [ MediaQuery.mobile ]
        [ borderRadius (px 8)
        ]
    ]


{-| Provide a list of custom HTML.
-}
html : List (Html msg) -> Attribute msg
html =
    Attribute << Content.html


{-| Provide a plain-text string.
-}
plaintext : String -> Attribute msg
plaintext =
    Attribute << Content.plaintext


{-| Provide a string that will be rendered as markdown.

Note that you may need to remove extra margin added by default
to `p` tags by user agents.

-}
markdown : String -> Attribute msg
markdown =
    Attribute << Content.markdown
