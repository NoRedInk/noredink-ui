module Nri.Ui.Heading.V3 exposing
    ( h1, h2, h3, h4, h5
    , Attribute
    , top, subhead, small
    , custom, css, nriDescription, testId, id
    )

{-|


# Changes from V2:

    - changes default h2 style to subhead
    - remove `customAttr`
    - remove `error` and `errorIf`
    - replaces `style` with `top`, `subhead`, and `small`

Headings with customization options for accessibility.

@docs h1, h2, h3, h4, h5

@docs Attribute
@docs top, subhead, small
@docs custom, css, nriDescription, testId, id

-}

import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attributes
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Html.Attributes.V2 as ExtraAttributes


{-| Make a first-level heading (styled like a top-level heading by default.)
-}
h1 : List (Attribute msg) -> List (Html msg) -> Html msg
h1 attributes =
    view Html.Styled.h1 (top :: attributes)


{-| Make a second-level heading (styled like a tagline by default.)
-}
h2 : List (Attribute msg) -> List (Html msg) -> Html msg
h2 attributes =
    view Html.Styled.h2 (subhead :: attributes)


{-| Make a third-level heading (styled like a subhead by default.)
-}
h3 : List (Attribute msg) -> List (Html msg) -> Html msg
h3 attributes =
    view Html.Styled.h3 (small :: attributes)


{-| Make a fourth-level heading (styled like a small heading by default.)
-}
h4 : List (Attribute msg) -> List (Html msg) -> Html msg
h4 attributes =
    view Html.Styled.h4 (small :: attributes)


{-| Make a fifth-level heading (styled like a small heading by default.)
-}
h5 : List (Attribute msg) -> List (Html msg) -> Html msg
h5 attributes =
    view Html.Styled.h5 (small :: attributes)


view :
    (List (Html.Styled.Attribute msg) -> List (Html msg) -> Html msg)
    -> List (Attribute msg)
    -> List (Html msg)
    -> Html msg
view tag attrs content =
    let
        final =
            List.foldl customize emptyCustomizations attrs
    in
    tag (Attributes.css final.css :: final.attributes) content


{-| Like an `Html.Attribute msg`, but specifically for headings. Use things
like `style` in this module to construct an Attribute.
-}
type Attribute msg
    = Css (List Css.Style)
    | Attributes_ (List (Html.Styled.Attribute msg))


{-| Set some custom CSS in this heading. For example, maybe you need to tweak
margins.
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


emptyCustomizations : Customizations msg
emptyCustomizations =
    { css = []
    , attributes = []
    }


type alias Customizations msg =
    { css : List Css.Style
    , attributes : List (Html.Styled.Attribute msg)
    }


customize : Attribute msg -> Customizations msg -> Customizations msg
customize attr customizations =
    case attr of
        Css css_ ->
            { customizations | css = customizations.css ++ css_ }

        Attributes_ attributes ->
            { customizations | attributes = customizations.attributes ++ attributes }



-- Style


{-| `top` headings are Colors.navy and have:

    font-size: 30px
    line-height: 38px
    font-weight: 700

By default.

-}
top : Attribute msg
top =
    (css << headingStyles)
        { color = Colors.navy
        , size = 30
        , lineHeight = 38
        }


{-| `subhead` headings are Colors.navy and have:

    font-size: 20px
    line-height: 26px
    font-weight: 700

By default.

-}
subhead : Attribute msg
subhead =
    (css << headingStyles)
        { color = Colors.navy
        , size = 20
        , lineHeight = 26
        }


{-| `small` headings are Colors.gray20 and have:

    font-size: 16px
    line-height: 21px
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
                , lineHeight = 21
                }
        )


headingStyles :
    { color : Color
    , lineHeight : Float
    , size : Float
    }
    -> List Css.Style
headingStyles config =
    [ Fonts.baseFont
    , fontSize (px config.size)
    , color config.color
    , lineHeight (px config.lineHeight)
    , fontWeight (int 700)
    , padding zero
    , textAlign left
    , margin zero
    ]
