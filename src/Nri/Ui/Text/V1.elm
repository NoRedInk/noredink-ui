module Nri.Ui.Text.V1
    exposing
        ( caption
        , captionClassString
        , heading
        , mediumBody
        , mediumBodyClassString
        , noWidow
        , smallBody
        , smallBodyClassString
        , smallBodyGray
        , smallHeading
        , styles
        , subHeading
        , tagline
        , ugMediumBody
        , ugSmallBody
        )

{-|

@docs styles


## Semantic text types:

@docs caption, heading, mediumBody, smallBody, smallBodyGray, subHeading, smallHeading, tagline


## User-generated text styles:

@docs ugMediumBody,ugSmallBody


## Text class strings:

@docs captionClassString, mediumBodyClassString, smallBodyClassString


## Modifying strings to display nicely:

@docs noWidow

-}

import Css exposing (..)
import Css.Foreign exposing (Snippet, children, descendants, everything, selector)
import Css.Helpers exposing (identifierToString)
import Html exposing (..)
import Html.Styled
import Html.Styled.Attributes exposing (css)
import Nri.Colors exposing (..)
import Nri.Stylers exposing (makeFont)
import Nri.Ui.Styles.V1


{-| This is a Page Heading.
-}
heading : List (Html msg) -> Html msg
heading content =
    h1 [ class [ Heading, Text ] ] content


{-| This is a tagline for a page heading.
-}
tagline : List (Html msg) -> Html msg
tagline content =
    h2 [ class [ Tagline, Text ] ] content


{-| This is a subhead.
-}
subHeading : List (Html msg) -> Html msg
subHeading content =
    h3 [ class [ SubHeading, Text ] ] content


{-| This is a small Page Heading.
-}
smallHeading : List (Html msg) -> Html msg
smallHeading content =
    h4 [ class [ SmallHeading, Text ] ] content


{-| This is some medium body copy.
-}
mediumBody : List (Html msg) -> Html msg
mediumBody content =
    p [ class mediumBodyClasses ] content


mediumBodyClasses : List CssClasses
mediumBodyClasses =
    [ MediumBody, Text ]


{-| Stringified version of `mediumBodyClasses`.
Necessary because <https://github.com/rtfeldman/elm-css-helpers/issues/2>
-}
mediumBodyClassString : String
mediumBodyClassString =
    classString mediumBodyClasses


{-| This is some small body copy.
-}
smallBody : List (Html msg) -> Html msg
smallBody content =
    p [ class smallBodyClasses ] content


smallBodyClasses : List CssClasses
smallBodyClasses =
    [ SmallBody, Text ]


{-| Stringified version of `smallBodyClasses`.
Necessary because <https://github.com/rtfeldman/elm-css-helpers/issues/2>
-}
smallBodyClassString : String
smallBodyClassString =
    classString smallBodyClasses


{-| This is some small body copy but it's gray.
-}
smallBodyGray : List (Html msg) -> Html msg
smallBodyGray content =
    p [ class [ SmallBodyGray, Text ] ] content


{-| This is a little note or caption.
-}
caption : List (Html msg) -> Html msg
caption content =
    p [ class captionClasses ] content


captionClasses : List CssClasses
captionClasses =
    [ Caption ]


{-| Stringified version of `captionClasses`.
Necessary because <https://github.com/rtfeldman/elm-css-helpers/issues/2>
-}
captionClassString : String
captionClassString =
    classString captionClasses


classString : List CssClasses -> String
classString classes =
    String.join " " (List.map (identifierToString namespace) classes)


{-| User-generated text.
-}
ugMediumBody : List (Html.Styled.Html msg) -> Html.Styled.Html msg
ugMediumBody =
    Html.Styled.p
        [ css []
        ]


{-| User-generated text.
-}
ugSmallBody : List (Html.Styled.Html msg) -> Html.Styled.Html msg
ugSmallBody =
    Html.Styled.p
        [ css []
        ]


namespace : String
namespace =
    "Nri-Ui-Text-"


type CssClasses
    = Text
    | Heading
    | Tagline
    | SubHeading
    | SmallHeading
    | MediumBody
    | SmallBody
    | SmallBodyGray
    | Caption


{-| -}
styles : Nri.Ui.Styles.V1.Styles Never CssClasses msg
styles =
    Nri.Ui.Styles.V1.styles namespace
        [ Css.Foreign.class Heading
            [ makeFont (px 30) navy
            , lineHeight (px 40.5)
            , fontWeight (int 700)
            , margin zero
            ]
        , Css.Foreign.class Tagline
            [ makeFont (px 20) gray45
            , lineHeight (px 27)
            , fontWeight (int 400)
            , margin4 (px 5) (px 0) (px 0) (px 0)
            ]
        , Css.Foreign.class SubHeading
            [ makeFont (px 20) navy
            , lineHeight (px 27)
            , fontWeight (int 700)
            , margin4 (px 20) (px 0) (px 10) (px 0)
            ]
        , Css.Foreign.class SmallHeading
            [ makeFont (px 16) gray20
            , lineHeight (px 23)
            , fontWeight (int 700)
            , margin zero
            ]
        , Css.Foreign.class MediumBody
            [ makeFont (px 18) gray20
            , lineHeight (px 27)
            , fontWeight (int 400)
            , margin4 (px 10) (px 0) (px 0) (px 0)
            ]
        , Css.Foreign.class SmallBody
            [ makeFont (px 15) gray20
            , lineHeight (px 23)
            , fontWeight (int 400)
            , margin4 (px 7) (px 0) (px 0) (px 0)
            ]
        , Css.Foreign.class SmallBodyGray
            [ makeFont (px 15) gray45
            , lineHeight (px 23)
            , fontWeight (int 400)
            , margin4 (px 7) (px 0) (px 0) (px 0)
            ]
        , Css.Foreign.class Caption
            [ makeFont (px 13) gray45
            , lineHeight (px 18)
            , fontWeight (int 400)
            , margin4 (px 5) (px 0) (px 0) (px 0)
            ]
        , Css.Foreign.class Text
            [ padding zero
            , textAlign left
            , firstChild
                [ margin zero
                ]
            ]
        ]


{ class } =
    styles


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
