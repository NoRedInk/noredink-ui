module Nri.Ui.Text.V5 exposing
    ( caption, mediumBody, mediumBodyGray, smallBody, smallBodyGray
    , ugMediumBody, ugSmallBody
    , Attribute, noBreak, css
    , noWidow
    )

{-| Post-release patches

  - adjusts link styles

Changes from V3:

  - Removes Headings (they now live in Nri.Ui.Heading.V2)

Changes from V4:

  - Added `Attribute` for customizing styles


## Understanding spacing

  - All text styles have a specific line-height. This is set so that when text in the given style
    is long enough to wrap, the spacing between wrapped lines looks good.
  - No text styles have padding.
  - **Heading styles** do not have margin. It is up to the caller to add appropriate margin to the layout.
  - **Paragraph styles** only have bottom margin, but with **:last-child bottom margin set to zero**.
    This bottom margin is set to look good when multiple paragraphs of the same style follow one another.
      - If you want content after the paragraph and don't want the margin, put the paragraph in a `div` so that it will be the last-child, which will get rid of the bottom margin.
  - **User-authored content blocks** preserve line breaks and do not have margin.


## Headings

Headings now live in Nri.Ui.Heading.V2. Here's a mapping to help with upgrades:

    | Nri.Ui.Text.V3    | Nri.Ui.Heading.V2 |
    |===================|===================|
    | Text.heading      | Heading.h1        |
    | Text.tagline      | Heading.h2        |
    | Text.subHeading   | Heading.h3        |
    | Text.smallHeading | Heading.h4        |

If you look at your new code and go "hmm, those shouldn't be at this level of
heading" then you can customize the tag apart from the style using the new
API. See the Nri.Ui.Heading.V2 docs for details.


## Paragraph styles

@docs caption, mediumBody, mediumBodyGray, smallBody, smallBodyGray


## User-authored content blocks:

@docs ugMediumBody, ugSmallBody


## Customizations

@docs Attribute, noBreak, css


## Modifying strings to display nicely:

@docs noWidow

-}

import Css exposing (..)
import Css.Global exposing (a, descendants)
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attrs
import Nri.Ui.Colors.V1 exposing (..)
import Nri.Ui.Fonts.V1 as Fonts


{-| -}
type Attribute
    = NoBreak
    | Css (List Style)


{-| Text with this attribute will never wrap.
-}
noBreak : Attribute
noBreak =
    NoBreak


{-| Add some custom CSS to the text. If you find yourself using this a lot,
please add a stricter attribute to noredink-ui!
-}
css : List Style -> Attribute
css =
    Css


styleForAttributes : List Attribute -> Style
styleForAttributes attrs =
    let
        config =
            List.foldl
                (\attr soFar ->
                    case attr of
                        NoBreak ->
                            { soFar | noBreak = True }

                        Css styles ->
                            { soFar | styles = soFar.styles ++ styles }
                )
                { noBreak = False
                , styles = []
                }
                attrs
    in
    batch
        [ if config.noBreak then
            whiteSpace noWrap

          else
            batch []
        , batch config.styles
        ]


{-| This is some medium body copy.
-}
mediumBody : List Attribute -> List (Html msg) -> Html msg
mediumBody attributes content =
    p
        [ paragraphStyles
            attributes
            { font = Fonts.baseFont
            , color = gray20
            , size = 18
            , lineHeight = 28
            , weight = 400
            , margin = 10
            }
        ]
        content


{-| `mediumBody`, but with a lighter gray color than the default.
-}
mediumBodyGray : List Attribute -> List (Html msg) -> Html msg
mediumBodyGray attributes content =
    mediumBody (css [ Css.color gray45 ] :: attributes) content


{-| This is some small body copy.
-}
smallBody : List Attribute -> List (Html msg) -> Html msg
smallBody attributes content =
    p
        [ paragraphStyles
            attributes
            { font = Fonts.baseFont
            , color = gray20
            , size = 15
            , lineHeight = 23
            , weight = 400
            , margin = 7
            }
        ]
        content


{-| This is some small body copy but it's gray.
-}
smallBodyGray : List Attribute -> List (Html msg) -> Html msg
smallBodyGray attributes content =
    p
        [ paragraphStyles
            attributes
            { font = Fonts.baseFont
            , color = gray45
            , size = 15
            , lineHeight = 23
            , weight = 400
            , margin = 7
            }
        ]
        content


paragraphStyles :
    List Attribute
    ->
        { color : Color
        , font : Style
        , lineHeight : Float
        , margin : Float
        , size : Float
        , weight : Int
        }
    -> Html.Styled.Attribute msg
paragraphStyles attributes config =
    Attrs.css
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
        , styleForAttributes attributes
        ]


{-| This is a little note or caption.
-}
caption : List Attribute -> List (Html msg) -> Html msg
caption attributes content =
    p
        [ paragraphStyles
            attributes
            { font = Fonts.baseFont
            , color = gray45
            , size = 13
            , lineHeight = 18
            , weight = 400
            , margin = 5
            }
        ]
        content


{-| User-generated text.
-}
ugMediumBody : List Attribute -> List (Html msg) -> Html msg
ugMediumBody attributes =
    p
        [ Attrs.css
            [ Fonts.quizFont
            , fontSize (px 18)
            , lineHeight (px 30)
            , whiteSpace preLine
            , color gray20
            , margin zero
            , styleForAttributes attributes
            ]
        ]


{-| User-generated text.
-}
ugSmallBody : List Attribute -> List (Html msg) -> Html msg
ugSmallBody attributes =
    p
        [ Attrs.css
            [ Fonts.quizFont
            , fontSize (px 16)
            , lineHeight (px 25)
            , whiteSpace preLine
            , color gray20
            , margin zero
            , styleForAttributes attributes
            ]
        ]


{-| Eliminate widows (single words on their own line caused by
wrapping) by inserting a non-breaking space if there are at least two
words.
-}
noWidow : String -> String
noWidow inputs =
    let
        -- this value is a unicode non-breaking space since Elm
        -- doesn't support named character entities
        nbsp =
            "\u{00A0}"

        words =
            String.split " " inputs

        insertPoint =
            List.length words - 1
    in
    words
        |> List.indexedMap
            (\i word ->
                if i == 0 then
                    word

                else if i == insertPoint && insertPoint > 0 then
                    nbsp ++ word

                else
                    " " ++ word
            )
        |> String.join ""
