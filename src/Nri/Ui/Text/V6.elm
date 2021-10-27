module Nri.Ui.Text.V6 exposing
    ( caption, mediumBody, mediumBodyGray, smallBody, smallBodyGray
    , ugMediumBody, ugSmallBody
    , Attribute, noBreak, css, id, custom
    , nriDescription, testId
    , noWidow
    )

{-| Changes from V5:

  - adds helpers: `custom`, `nriDescription`,`testId`,`id`


## Understanding spacing

  - All text styles have a specific line-height. This is set so that when text in the given style
    is long enough to wrap, the spacing between wrapped lines looks good.
  - No text styles have padding.
  - **Paragraph styles** only have bottom margin, but with **:last-child bottom margin set to zero**.
    This bottom margin is set to look good when multiple paragraphs of the same style follow one another.
      - If you want content after the paragraph and don't want the margin, put the paragraph in a `div` so that it will be the last-child, which will get rid of the bottom margin.
  - **User-authored content blocks** preserve line breaks and do not have margin.


## Headings

You're in the wrong place! Headings live in Nri.Ui.Heading.V2.


## Paragraph styles

@docs caption, mediumBody, mediumBodyGray, smallBody, smallBodyGray


## User-authored content blocks:

@docs ugMediumBody, ugSmallBody


## Customizations

@docs Attribute, noBreak, css, id, custom
@docs nriDescription, testId


## Modifying strings to display nicely:

@docs noWidow

-}

import Accessibility.Styled as Html exposing (..)
import Css exposing (..)
import Css.Global exposing (a, descendants)
import Html.Styled.Attributes as Attributes
import Nri.Ui.Colors.V1 exposing (..)
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Html.Attributes.V2 as ExtraAttributes


{-| -}
type Attribute
    = Attribute (Settings -> Settings)


type alias Settings =
    { noBreak : Bool
    , styles : List Css.Style
    , customAttributes : List (Html.Attribute Never)
    }


defaultSettings : Settings
defaultSettings =
    { noBreak = False
    , styles = []
    , customAttributes = []
    }


buildSettings : List Attribute -> Settings
buildSettings =
    List.foldl (\(Attribute f) acc -> f acc) defaultSettings


{-| Text with this attribute will never wrap.
-}
noBreak : Attribute
noBreak =
    Attribute (\config -> { config | noBreak = True })


{-| Use this helper to add custom attributes.

Do NOT use this helper to add css styles, as they may not be applied the way
you want/expect if underlying styles change.
Instead, please use the `css` helper.

-}
custom : List (Html.Attribute Never) -> Attribute
custom attributes =
    Attribute <|
        \config ->
            { config
                | customAttributes = List.append config.customAttributes attributes
            }


{-| -}
nriDescription : String -> Attribute
nriDescription description =
    custom [ ExtraAttributes.nriDescription description ]


{-| -}
testId : String -> Attribute
testId id_ =
    custom [ ExtraAttributes.testId id_ ]


{-| -}
id : String -> Attribute
id id_ =
    custom [ Attributes.id id_ ]


{-| Add some custom CSS to the text. If you find yourself using this a lot,
please add a stricter attribute to noredink-ui!
-}
css : List Style -> Attribute
css styles =
    Attribute (\config -> { config | styles = config.styles ++ styles })


{-| -}
view : List Attribute -> List (Html msg) -> Html msg
view attributes content =
    let
        settings : Settings
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
        content


{-| This is some medium body copy.
-}
mediumBody : List Attribute -> List (Html msg) -> Html msg
mediumBody attributes content =
    let
        settings : Settings
        settings =
            buildSettings attributes
    in
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
        content


{-| This is some small body copy but it's gray.
-}
smallBodyGray : List Attribute -> List (Html msg) -> Html msg
smallBodyGray attributes content =
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
        content


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
caption : List Attribute -> List (Html msg) -> Html msg
caption attributes content =
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
        content


{-| User-generated text.
-}
ugMediumBody : List Attribute -> List (Html msg) -> Html msg
ugMediumBody attributes content =
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
        content


{-| User-generated text.
-}
ugSmallBody : List Attribute -> List (Html msg) -> Html msg
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
