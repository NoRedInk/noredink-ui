module Nri.Ui.Text.V6 exposing
    ( caption, mediumBody, mediumBodyGray, smallBody, smallBodyGray
    , footnote
    , ugMediumBody, ugSmallBody
    , paragraph, plaintext, markdown, html
    , Attribute, noBreak, css, id, custom
    , nriDescription, testId
    )

{-| Patch changes:

  - use internal `Content` module
  - adds paragraph

Changes from V5:

  - adds helpers: `custom`, `nriDescription`,`testId`,`id`
  - instead of view helpers that take HTML, offer attribute helpers supporting plaintext, markdown, and html content
  - :skull: remove noWidow, which is not used
  - noBreak now takes a bool


## Understanding spacing

  - All text styles have a specific line-height. This is set so that when text in the given style
    is long enough to wrap, the spacing between wrapped lines looks good.
  - No text styles have padding.
  - **Paragraph styles** only have bottom margin, but with **:last-child bottom margin set to zero**.
    This bottom margin is set to look good when multiple paragraphs of the same style follow one another.
      - If you want content after the paragraph and don't want the margin, put the paragraph in a `div` so that it will be the last-child, which will get rid of the bottom margin.
  - **User-authored content blocks** preserve line breaks and do not have margin.


## Headings

You're in the wrong place! Headings live in Nri.Ui.Heading.V3.


## Paragraph styles

@docs caption, mediumBody, mediumBodyGray, smallBody, smallBodyGray
@docs footnote


## User-authored content blocks:

@docs ugMediumBody, ugSmallBody


# Content

@docs paragraph, plaintext, markdown, html


## Customizations

@docs Attribute, noBreak, css, id, custom
@docs nriDescription, testId

-}

import Accessibility.Styled as Html exposing (..)
import Content
import Css exposing (..)
import Css.Global
import Html.Styled.Attributes as Attributes
import Nri.Ui.Colors.V1 exposing (..)
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Html.Attributes.V2 as ExtraAttributes


{-| -}
type Attribute msg
    = Attribute (Settings msg -> Settings msg)


type alias Settings msg =
    { noBreak : Bool
    , styles : List Css.Style
    , customAttributes : List (Html.Attribute Never)
    , content : List (Html msg)
    }


defaultSettings : Settings msg
defaultSettings =
    { noBreak = False
    , styles = []
    , customAttributes = []
    , content = []
    }


buildSettings : List (Attribute msg) -> Settings msg
buildSettings =
    List.foldl (\(Attribute f) acc -> f acc) defaultSettings


{-| Pass True to prevent text from ever wrapping.

The default Text behavior is `noBreak False`, which means content will wrap.

-}
noBreak : Bool -> Attribute msg
noBreak noBreak_ =
    Attribute (\config -> { config | noBreak = noBreak_ })


{-| Use this helper to add custom attributes.

Do NOT use this helper to add css styles, as they may not be applied the way
you want/expect if underlying styles change.
Instead, please use the `css` helper.

-}
custom : List (Html.Attribute Never) -> Attribute msg
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


{-| -}
view : List (Attribute msg) -> Html msg
view attributes =
    let
        settings : Settings msg
        settings =
            buildSettings attributes
    in
    p
        (Attributes.css
            [ if settings.noBreak then
                whiteSpace noWrap

              else
                batch []
            , batch settings.styles
            ]
            :: settings.customAttributes
        )
        settings.content


{-| This is some medium body copy.
-}
mediumBody : List (Attribute msg) -> Html msg
mediumBody attributes =
    view
        (css
            (paragraphStyles
                { font = Fonts.baseFont
                , color = gray20
                , size = 18
                , lineHeight = 28
                , weight = 400
                , margin = 10
                }
            )
            :: attributes
        )


{-| `mediumBody`, but with a lighter gray color than the default.
-}
mediumBodyGray : List (Attribute msg) -> Html msg
mediumBodyGray attributes =
    mediumBody (css [ Css.color gray45 ] :: attributes)


{-| This is some small body copy.
-}
smallBody : List (Attribute msg) -> Html msg
smallBody attributes =
    view
        (css
            (paragraphStyles
                { font = Fonts.baseFont
                , color = gray20
                , size = 15
                , lineHeight = 23
                , weight = 400
                , margin = 7
                }
            )
            :: attributes
        )


{-| This is some small body copy but it's gray.
-}
smallBodyGray : List (Attribute msg) -> Html msg
smallBodyGray attributes =
    view
        (css
            (paragraphStyles
                { font = Fonts.baseFont
                , color = gray45
                , size = 15
                , lineHeight = 23
                , weight = 400
                , margin = 7
                }
            )
            :: attributes
        )


paragraphStyles :
    { color : Color
    , font : Style
    , lineHeight : Float
    , margin : Float
    , size : Float
    , weight : Int
    }
    -> List Css.Style
paragraphStyles config =
    [ config.font
    , fontSize (px config.size)
    , color config.color
    , lineHeight (px config.lineHeight)
    , fontWeight (int config.weight)
    , padding zero
    , textAlign left
    , margin4 (px 0) (px 0) (px config.margin) (px 0)
    , Css.Global.descendants
        [ Css.Global.a
            [ textDecoration none
            , color azure
            , borderBottom3 (px 1) solid azure
            , visited
                [ color azure ]
            ]
        ]
    , lastChild
        [ margin zero
        ]
    ]


{-| This is a little note or caption.
-}
caption : List (Attribute msg) -> Html msg
caption attributes =
    view
        (css
            (paragraphStyles
                { font = Fonts.baseFont
                , color = gray45
                , size = 13
                , lineHeight = 18
                , weight = 400
                , margin = 5
                }
            )
            :: attributes
        )


{-| User-generated text.
-}
ugMediumBody : List (Attribute msg) -> Html msg
ugMediumBody attributes =
    view
        (css
            (whiteSpace preLine
                :: paragraphStyles
                    { font = Fonts.quizFont
                    , color = gray20
                    , size = 18
                    , lineHeight = 30
                    , weight = 400
                    , margin = 0
                    }
            )
            :: attributes
        )


{-| User-generated text.
-}
ugSmallBody : List (Attribute msg) -> Html msg
ugSmallBody attributes =
    view
        (css
            (whiteSpace preLine
                :: paragraphStyles
                    { font = Fonts.quizFont
                    , color = gray20
                    , size = 16
                    , lineHeight = 25
                    , weight = 400
                    , margin = 0
                    }
            )
            :: attributes
        )


{-| This is a little note or footnote. Please be aware that under the hood, this is only a paragraph tag. If you're actually citing a resource, wrap this in the [`cite` HTML element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/cite).
-}
footnote : List (Attribute msg) -> Html msg
footnote attributes =
    view
        (css
            (Css.important (marginTop (Css.px 5))
                :: paragraphStyles
                    { font = Fonts.quizFont
                    , color = gray45
                    , size = 13
                    , lineHeight = 18
                    , weight = 400
                    , margin = 0
                    }
            )
            :: attributes
        )


{-| Provide a plain-text string.
-}
plaintext : String -> Attribute msg
plaintext =
    Attribute << Content.plaintext


{-| Provide a plain-text string that will be put into a paragraph tag, with the default margin removed.
-}
paragraph : String -> Attribute msg
paragraph =
    Attribute << Content.paragraph


{-| Provide a string that will be rendered as markdown.
-}
markdown : String -> Attribute msg
markdown =
    Attribute << Content.markdown


{-| Provide a list of custom HTML.
-}
html : List (Html msg) -> Attribute msg
html =
    Attribute << Content.html
