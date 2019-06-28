module Nri.Ui.Text.V3 exposing
    ( heading, subHeading, smallHeading, tagline
    , caption, mediumBody, smallBody, smallBodyGray
    , ugMediumBody, ugSmallBody
    , noWidow
    )

{-| Changes from V2:

  - Normalizes how margin is applied (see "Understanding spacing", below).


## Understanding spacing

  - All text styles have a specific line-height. This is set so that when text in the given style
    is long enough to wrap, the spacing between wrapped lines looks good.
  - No text styles have padding.
  - **Heading styles** do not have margin. It is up to the caller to add appropriate margin to the layout.
  - **Paragraph styles** only have bottom margin, but with **:last-child bottom margin set to zero**.
    This bottom margin is set to look good when multiple paragraphs of the same style follow one another.
      - If you want content after the paragraph and don't want the margin, put the paragraph in a `div` so that it will be the last-child, which will get rid of the bottom margin.
  - **User-authored content blocks** preserve line breaks and do not have margin.


## Heading styles

@docs heading, subHeading, smallHeading, tagline


## Paragraph styles

@docs caption, mediumBody, smallBody, smallBodyGray


## User-authored content blocks:

@docs ugMediumBody, ugSmallBody


## Modifying strings to display nicely:

@docs noWidow

-}

import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Nri.Ui.Colors.V1 exposing (..)
import Nri.Ui.Fonts.V1 as Fonts


{-| This is a Page Heading.
-}
heading : List (Html msg) -> Html msg
heading content =
    h1
        [ headingStyles
            { font = Fonts.baseFont
            , color = navy
            , size = 30
            , lineHeight = 40.5
            , weight = 700
            }
        ]
        content


{-| This is a tagline for a page heading.
-}
tagline : List (Html msg) -> Html msg
tagline content =
    h2
        [ headingStyles
            { font = Fonts.baseFont
            , color = gray45
            , size = 20
            , lineHeight = 27
            , weight = 400
            }
        ]
        content


{-| This is a subhead.
-}
subHeading : List (Html msg) -> Html msg
subHeading content =
    h3
        [ headingStyles
            { font = Fonts.baseFont
            , color = navy
            , size = 20
            , lineHeight = 27
            , weight = 700
            }
        ]
        content


{-| This is a small Page Heading.
-}
smallHeading : List (Html msg) -> Html msg
smallHeading content =
    h4
        [ headingStyles
            { font = Fonts.baseFont
            , color = gray20
            , size = 16
            , lineHeight = 23
            , weight = 700
            }
        ]
        content


{-| This is some medium body copy.
-}
mediumBody : List (Html msg) -> Html msg
mediumBody content =
    p
        [ paragraphStyles
            { font = Fonts.baseFont
            , color = gray20
            , size = 18
            , lineHeight = 27
            , weight = 400
            , margin = 10
            }
        ]
        content


{-| This is some small body copy.
-}
smallBody : List (Html msg) -> Html msg
smallBody content =
    p
        [ paragraphStyles
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
smallBodyGray : List (Html msg) -> Html msg
smallBodyGray content =
    p
        [ paragraphStyles
            { font = Fonts.baseFont
            , color = gray45
            , size = 15
            , lineHeight = 23
            , weight = 400
            , margin = 7
            }
        ]
        content


headingStyles config =
    css
        [ config.font
        , fontSize (px config.size)
        , color config.color
        , lineHeight (px config.lineHeight)
        , fontWeight (int config.weight)
        , padding zero
        , textAlign left
        , margin zero
        ]


paragraphStyles config =
    css
        [ config.font
        , fontSize (px config.size)
        , color config.color
        , lineHeight (px config.lineHeight)
        , fontWeight (int config.weight)
        , padding zero
        , textAlign left
        , margin4 (px 0) (px 0) (px config.margin) (px 0)
        , lastChild
            [ margin zero
            ]
        ]


{-| This is a little note or caption.
-}
caption : List (Html msg) -> Html msg
caption content =
    p
        [ paragraphStyles
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
ugMediumBody : List (Html msg) -> Html msg
ugMediumBody =
    p
        [ css
            [ Fonts.quizFont
            , fontSize (px 18)
            , lineHeight (px 30)
            , whiteSpace preLine
            , color gray20
            , margin zero
            ]
        ]


{-| User-generated text.
-}
ugSmallBody : List (Html msg) -> Html msg
ugSmallBody =
    p
        [ css
            [ Fonts.quizFont
            , fontSize (px 16)
            , lineHeight (px 25)
            , whiteSpace preLine
            , color gray20
            , margin zero
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
