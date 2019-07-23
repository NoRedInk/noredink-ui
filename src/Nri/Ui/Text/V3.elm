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

Please use `Nri.Ui.Heading.V1` instead of these in new code. If you're here to
make a new Text version, please remove them.

@docs heading, subHeading, smallHeading, tagline


## Paragraph styles

@docs caption, mediumBody, smallBody, smallBodyGray


## User-authored content blocks:

@docs ugMediumBody, ugSmallBody


## Modifying strings to display nicely:

@docs noWidow

-}

import Html.Styled exposing (Html)
import Nri.Ui.Heading.V1 as Heading
import Nri.Ui.Text.V4 as V4


{-| This is a Page Heading.
-}
heading : List (Html msg) -> Html msg
heading content =
    Heading.heading content
        |> Heading.withVisualLevel Heading.Top
        |> Heading.withDocumentLevel Heading.H1
        |> Heading.view


{-| This is a tagline for a page heading.
-}
tagline : List (Html msg) -> Html msg
tagline content =
    Heading.heading content
        |> Heading.withVisualLevel Heading.Tagline
        |> Heading.withDocumentLevel Heading.H2
        |> Heading.view


{-| This is a subhead.
-}
subHeading : List (Html msg) -> Html msg
subHeading content =
    Heading.heading content
        |> Heading.withVisualLevel Heading.Subhead
        |> Heading.withDocumentLevel Heading.H3
        |> Heading.view


{-| This is a small Page Heading.
-}
smallHeading : List (Html msg) -> Html msg
smallHeading content =
    Heading.heading content
        |> Heading.withVisualLevel Heading.Small
        |> Heading.withDocumentLevel Heading.H4
        |> Heading.view


{-| This is some medium body copy.
-}
mediumBody : List (Html msg) -> Html msg
mediumBody =
    V4.mediumBody


{-| This is some small body copy.
-}
smallBody : List (Html msg) -> Html msg
smallBody =
    V4.smallBody


{-| This is some small body copy but it's gray.
-}
smallBodyGray : List (Html msg) -> Html msg
smallBodyGray =
    V4.smallBodyGray


{-| This is a little note or caption.
-}
caption : List (Html msg) -> Html msg
caption =
    V4.caption


{-| User-generated text.
-}
ugMediumBody : List (Html msg) -> Html msg
ugMediumBody =
    V4.ugMediumBody


{-| User-generated text.
-}
ugSmallBody : List (Html msg) -> Html msg
ugSmallBody =
    V4.ugSmallBody


{-| Eliminate widows (single words on their own line caused by
wrapping) by inserting a non-breaking space if there are at least two
words.
-}
noWidow : String -> String
noWidow =
    V4.noWidow
