module Nri.Ui.Heading.V3 exposing
    ( h1, h2, h3, h4, h5
    , Attribute, style, Style(..)
    , custom, css, nriDescription, testId, id
    , customAttr
    )

{-|


# Patch changes:

    - changes default h2 style to subhead

Headings with customization options for accessibility.

@docs h1, h2, h3, h4, h5

@docs Attribute, style, Style, error, errorIf

@docs custom, css, nriDescription, testId, id
@docs customAttr

-}

import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attributes
import Nri.Ui.Colors.V1 exposing (..)
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Html.Attributes.V2 as ExtraAttributes


{-| Make a first-level heading (styled like a top-level heading by default.)
-}
h1 : List (Attribute msg) -> List (Html msg) -> Html msg
h1 =
    view Html.Styled.h1
        { style = Top
        , css = []
        , attributes = []
        }


{-| Make a second-level heading (styled like a tagline by default.)
-}
h2 : List (Attribute msg) -> List (Html msg) -> Html msg
h2 =
    view Html.Styled.h2
        { style = Subhead
        , css = []
        , attributes = []
        }


{-| Make a third-level heading (styled like a subhead by default.)
-}
h3 : List (Attribute msg) -> List (Html msg) -> Html msg
h3 =
    view Html.Styled.h3
        { style = Small
        , css = []
        , attributes = []
        }


{-| Make a fourth-level heading (styled like a small heading by default.)
-}
h4 : List (Attribute msg) -> List (Html msg) -> Html msg
h4 =
    view Html.Styled.h4
        { style = Small
        , css = []
        , attributes = []
        }


{-| Make a fifth-level heading (styled like a small heading by default.)
-}
h5 : List (Attribute msg) -> List (Html msg) -> Html msg
h5 =
    view Html.Styled.h5
        { style = Small
        , css = []
        , attributes = []
        }


view :
    (List (Html.Styled.Attribute msg) -> List (Html msg) -> Html msg)
    -> Customizations msg
    -> List (Attribute msg)
    -> List (Html msg)
    -> Html msg
view tag customizations attrs content =
    let
        final =
            List.foldl customize customizations attrs
    in
    tag
        (Attributes.css [ getStyles final.style, Css.batch final.css ]
            :: final.attributes
        )
        content


{-| Like an `Html.Attribute msg`, but specifically for headings. Use things
like `style` in this module to construct an Attribute.
-}
type Attribute msg
    = Style_ Style
    | Css (List Css.Style)
    | Attributes_ (List (Html.Styled.Attribute msg))
    | Skip


{-| -}
type Style
    = Top
    | Tagline
    | Subhead
    | Small


{-| Select which of the base styles this heading should look like. Each of h1..5
has a default, check their docs to see if you don't need to override this.
-}
style : Style -> Attribute msg
style =
    Style_


{-| Set some custom CSS in this heading. For example, maybe you need to tweak
margins. Now you can!
-}
css : List Css.Style -> Attribute msg
css =
    Css


{-| Set some custom attributes.

Please don't make headers interactive! Use buttons or links instead so that keyboard and screen
reader users can use the site too.

For style customizations, be sure to use the Heading.css helper.

-}
custom : List (Html.Styled.Attribute msg) -> Attribute msg
custom =
    Attributes_


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


{-| Please prefer `custom` for API consistency.
-}
customAttr : Html.Styled.Attribute msg -> Attribute msg
customAttr attr =
    Attributes_ [ attr ]


type alias Customizations msg =
    { style : Style
    , css : List Css.Style
    , attributes : List (Html.Styled.Attribute msg)
    }


customize : Attribute msg -> Customizations msg -> Customizations msg
customize attr customizations =
    case attr of
        Style_ newStyle ->
            { customizations | style = newStyle }

        Css css_ ->
            { customizations | css = customizations.css ++ css_ }

        Attributes_ attributes ->
            { customizations | attributes = customizations.attributes ++ attributes }

        Skip ->
            customizations



-- Style


getStyles : Style -> Css.Style
getStyles style_ =
    case style_ of
        Top ->
            headingStyles
                { font = Fonts.baseFont
                , color = navy
                , size = 30
                , lineHeight = 38
                , weight = 700
                }

        Tagline ->
            headingStyles
                { font = Fonts.baseFont
                , color = gray45
                , size = 20
                , lineHeight = 30
                , weight = 400
                }

        Subhead ->
            headingStyles
                { font = Fonts.baseFont
                , color = navy
                , size = 20
                , lineHeight = 26
                , weight = 700
                }

        Small ->
            Css.batch
                [ headingStyles
                    { font = Fonts.baseFont
                    , color = gray20
                    , size = 16
                    , lineHeight = 21
                    , weight = 700
                    }
                , letterSpacing (px -0.13)
                ]


headingStyles :
    { color : Color
    , font : Css.Style
    , lineHeight : Float
    , size : Float
    , weight : Int
    }
    -> Css.Style
headingStyles config =
    Css.batch
        [ config.font
        , fontSize (px config.size)
        , color config.color
        , lineHeight (px config.lineHeight)
        , fontWeight (int config.weight)
        , padding zero
        , textAlign left
        , margin zero
        ]
