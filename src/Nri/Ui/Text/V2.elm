module Nri.Ui.Text.V2 exposing
    ( caption, heading, mediumBody, smallBody, smallBodyGray, subHeading, smallHeading, tagline
    , ugMediumBody, ugSmallBody
    , noWidow
    )

{-|


## Semantic text types:

@docs caption, heading, mediumBody, smallBody, smallBodyGray, subHeading, smallHeading, tagline


## User-generated text styles:

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
        [ css
            (textStyles
                ++ [ Fonts.baseFont
                   , fontSize (px 30)
                   , color navy
                   , lineHeight (px 40.5)
                   , fontWeight (int 700)
                   , margin zero
                   ]
            )
        ]
        content


{-| This is a tagline for a page heading.
-}
tagline : List (Html msg) -> Html msg
tagline content =
    h2
        [ css
            (textStyles
                ++ [ Fonts.baseFont
                   , fontSize (px 20)
                   , color gray45
                   , lineHeight (px 27)
                   , fontWeight (int 400)
                   , margin4 (px 5) (px 0) (px 0) (px 0)
                   ]
            )
        ]
        content


{-| This is a subhead.
-}
subHeading : List (Html msg) -> Html msg
subHeading content =
    h3
        [ css
            (textStyles
                ++ [ Fonts.baseFont
                   , fontSize (px 20)
                   , color navy
                   , lineHeight (px 27)
                   , fontWeight (int 700)
                   , margin4 (px 20) (px 0) (px 10) (px 0)
                   ]
            )
        ]
        content


{-| This is a small Page Heading.
-}
smallHeading : List (Html msg) -> Html msg
smallHeading content =
    h4
        [ css
            (textStyles
                ++ [ Fonts.baseFont
                   , fontSize (px 16)
                   , color gray20
                   , lineHeight (px 23)
                   , fontWeight (int 700)
                   , margin zero
                   ]
            )
        ]
        content


{-| This is some medium body copy.
-}
mediumBody : List (Html msg) -> Html msg
mediumBody content =
    p
        [ css
            (textStyles
                ++ [ Fonts.baseFont
                   , fontSize (px 18)
                   , color gray20
                   , lineHeight (px 27)
                   , fontWeight (int 400)
                   , margin4 (px 10) (px 0) (px 0) (px 0)
                   ]
            )
        ]
        content


{-| This is some small body copy.
-}
smallBody : List (Html msg) -> Html msg
smallBody content =
    p
        [ css
            (textStyles
                ++ [ Fonts.baseFont
                   , fontSize (px 15)
                   , color gray20
                   , lineHeight (px 23)
                   , fontWeight (int 400)
                   , margin4 (px 7) (px 0) (px 0) (px 0)
                   ]
            )
        ]
        content


{-| This is some small body copy but it's gray.
-}
smallBodyGray : List (Html msg) -> Html msg
smallBodyGray content =
    p
        [ css
            (textStyles
                ++ [ Fonts.baseFont
                   , fontSize (px 15)
                   , color gray45
                   , lineHeight (px 23)
                   , fontWeight (int 400)
                   , margin4 (px 7) (px 0) (px 0) (px 0)
                   ]
            )
        ]
        content


textStyles =
    [ padding zero
    , textAlign left
    , firstChild
        [ margin zero
        ]
    ]


{-| This is a little note or caption.
-}
caption : List (Html msg) -> Html msg
caption content =
    p
        [ css
            [ Fonts.baseFont
            , fontSize (px 13)
            , color gray45
            , lineHeight (px 18)
            , fontWeight (int 400)
            , margin4 (px 5) (px 0) (px 0) (px 0)
            ]
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
            , margin4 (px 10) (px 0) (px 0) (px 0)
            , firstChild [ margin zero ]
            , firstOfType [ margin zero ]
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
            , margin4 (px 7) (px 0) (px 0) (px 0)
            , firstChild [ margin zero ]
            , firstOfType [ margin zero ]
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
            " "

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
