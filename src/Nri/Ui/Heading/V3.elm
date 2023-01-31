module Nri.Ui.Heading.V3 exposing
    ( h1, h2, h3, h4, h5
    , plaintext, markdown, html
    , Attribute
    , top, subhead, small
    , custom, css, nriDescription, testId, id
    )

{-|


# Changelog


## Patch changes

  - use internal `Content` module


## Changes from V2:

  - changes default h2 style to subhead
  - remove `customAttr`
  - remove `error` and `errorIf`
  - replaces `style` with `top`, `subhead`, and `small`
  - replaces list of HTML attributes with content approach (`plaintext`, `markdown`, `html`) used in Text

Headings with customization options.

@docs h1, h2, h3, h4, h5


# Content

@docs plaintext, markdown, html


## Customizations

@docs Attribute
@docs top, subhead, small
@docs custom, css, nriDescription, testId, id

-}

import Content
import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attributes
import MarkdownStyles
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Html.Attributes.V2 as ExtraAttributes


{-| Make a first-level heading (styled like a top-level heading by default.)
-}
h1 : List (Attribute msg) -> Html msg
h1 attributes =
    view Html.Styled.h1 (top :: attributes)


{-| Make a second-level heading (styled like a tagline by default.)
-}
h2 : List (Attribute msg) -> Html msg
h2 attributes =
    view Html.Styled.h2 (subhead :: attributes)


{-| Make a third-level heading (styled like a subhead by default.)
-}
h3 : List (Attribute msg) -> Html msg
h3 attributes =
    view Html.Styled.h3 (small :: attributes)


{-| Make a fourth-level heading (styled like a small heading by default.)
-}
h4 : List (Attribute msg) -> Html msg
h4 attributes =
    view Html.Styled.h4 (small :: attributes)


{-| Make a fifth-level heading (styled like a small heading by default.)
-}
h5 : List (Attribute msg) -> Html msg
h5 attributes =
    view Html.Styled.h5 (small :: attributes)


view :
    (List (Html.Styled.Attribute msg) -> List (Html msg) -> Html msg)
    -> List (Attribute msg)
    -> Html msg
view tag attrs =
    let
        final =
            List.foldl (\(Attribute f) acc -> f acc) emptyCustomizations attrs
    in
    tag (Attributes.css final.css :: final.attributes) final.content


{-| Provide a plain-text string.
-}
plaintext : String -> Attribute msg
plaintext =
    Attribute << Content.plaintext


{-| Provide a string that will be rendered as markdown.
-}
markdown : String -> Attribute msg
markdown content =
    Attribute <|
        \config ->
            { config
                | content = Content.markdownContent content
                , css = config.css ++ MarkdownStyles.anchorAndButton
            }


{-| Provide a list of custom HTML.
-}
html : List (Html msg) -> Attribute msg
html =
    Attribute << Content.html


{-| Like an `Html.Attribute msg`, but specifically for headings. Use things
like `style` in this module to construct an Attribute.
-}
type Attribute msg
    = Attribute (Customizations msg -> Customizations msg)


{-| Set some custom CSS in this heading. For example, maybe you need to tweak
margins.
-}
css : List Css.Style -> Attribute msg
css css_ =
    Attribute
        (\customizations ->
            { customizations | css = customizations.css ++ css_ }
        )


{-| Set some custom attributes.

Please don't make headers interactive! Use buttons or links instead so that keyboard and screen
reader users can use the site too.

For style customizations, be sure to use the Heading.css helper.

-}
custom : List (Html.Styled.Attribute msg) -> Attribute msg
custom attributes =
    Attribute
        (\customizations ->
            { customizations | attributes = customizations.attributes ++ attributes }
        )


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


emptyCustomizations : Customizations msg
emptyCustomizations =
    { content = []
    , css = []
    , attributes = []
    }


type alias Customizations msg =
    { content : List (Html msg)
    , css : List Css.Style
    , attributes : List (Html.Styled.Attribute msg)
    }



-- Style


{-| `top` headings are Colors.navy and have:

    font-size: 30px
    font-weight: 700

By default.

-}
top : Attribute msg
top =
    (css << headingStyles)
        { color = Colors.navy
        , size = 30
        }


{-| `subhead` headings are Colors.navy and have:

    font-size: 20px
    font-weight: 700

By default.

-}
subhead : Attribute msg
subhead =
    (css << headingStyles)
        { color = Colors.navy
        , size = 20
        }


{-| `small` headings are Colors.gray20 and have:

    font-size: 16px
    font-weight: 700

By default.

`small` heading default styles also make the [letter-spacing](https://developer.mozilla.org/en-US/docs/Web/CSS/letter-spacing) slightly narrower, by 0.13px.

-}
small : Attribute msg
small =
    css
        (letterSpacing (px -0.13)
            :: headingStyles
                { color = Colors.gray20
                , size = 16
                }
        )


headingStyles :
    { color : Color
    , size : Float
    }
    -> List Css.Style
headingStyles config =
    [ Fonts.baseFont
    , fontSize (px config.size)
    , color config.color
    , lineHeight (num 1.2)
    , fontWeight (int 700)
    , padding zero
    , textAlign left
    , margin zero
    ]
