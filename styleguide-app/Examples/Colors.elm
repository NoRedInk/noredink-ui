module Examples.Colors exposing
    ( example, State, Msg
    , all, backgroundHighlightColors
    )

{-|

@docs example, State, Msg
@docs all, backgroundHighlightColors

-}

import Category exposing (Category(..))
import Css
import Example exposing (Example)
import Html.Styled as Html
import Html.Styled.Attributes as Attributes exposing (css)
import Nri.Ui.Colors.Extra
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Heading.V3 as Heading
import Nri.Ui.Text.V6 as Text
import SolidColor exposing (highContrast)


type alias ColorExample =
    ( String, Css.Color, String )


type alias State =
    ()


{-| -}
type alias Msg =
    ()


{-| -}
example : Example State Msg
example =
    { name = "Colors"
    , version = 1
    , categories = [ Atoms ]
    , keyboardSupport = []
    , state = ()
    , update = \_ state -> ( state, Cmd.none )
    , subscriptions = \_ -> Sub.none
    , preview =
        [ ( "green", Colors.green )
        , ( "purple", Colors.purple )
        , ( "mustard", Colors.mustard )
        ]
            |> List.map viewPreviewSwatch
    , view =
        \ellieLinkConfig _ ->
            [ viewColors uncategorizedColors
            , Heading.h2 [ Heading.plaintext "Background Highlight Colors" ]
            , Text.mediumBody [ Text.plaintext "Background highlights should be used as the default highlight style because they are more noticeable and readable. The dark colors should be used in the case where headings need to harmonize with highlighted containers, such as in Guided Drafts." ]
            , viewColors backgroundHighlightColors
            , Heading.h2 [ Heading.plaintext "Text Highlight Colors" ]
            , Text.mediumBody [ Text.plaintext "Colors for highlighting text on a white background.  These colors are readable at 14px bold and bigger." ]
            , viewColors textHighlightColors
            ]
    }


all : List ( String, Css.Color )
all =
    [ uncategorizedColors
    , backgroundHighlightColors
    , textHighlightColors
    ]
        |> List.concat
        |> List.map (\( name, val, _ ) -> ( name, val ))


uncategorizedColors : List ( String, Css.Color, String )
uncategorizedColors =
    [ ( "gray20", Colors.gray20, "Main text" )
    , ( "gray45", Colors.gray45, "Secondary text, 0-69 score" )
    , ( "gray75", Colors.gray75, "Border of form elements and tabs" )
    , ( "gray85", Colors.gray85, "Alternate for divider lines and container borders" )
    , ( "gray92", Colors.gray92, "Dvdrs/rules, incomplete assmt, inactive tabs/dsbld buttons" )
    , ( "gray96", Colors.gray96, "backgrounds/alternating rows" )
    , ( "navy", Colors.navy, "Headings, indented compts, labels, tooltip bckgrnds" )
    , ( "azure", Colors.azure, "Buttons, other clickable stuff, links" )
    , ( "azureDark", Colors.azureDark, "Azure button shadow" )
    , ( "frost", Colors.frost, "Blue backgrounds pairing with Navy and Azure" )
    , ( "glacier", Colors.glacier, "Blue highlights/selected elements" )
    , ( "lichen", Colors.lichen, "70-79 score" )
    , ( "grassland", Colors.grassland, "80-89 score" )
    , ( "green", Colors.green, "90-100 score" )
    , ( "greenDark", Colors.greenDark, "Green button, swathes of green" )
    , ( "greenDarkest", Colors.greenDarkest, "Green text, green button shadow" )
    , ( "greenLight", Colors.greenLight, "Green backgrounds" )
    , ( "greenLightest", Colors.greenLightest, "Green backgrounds" )
    , ( "cornflower", Colors.cornflower, "Mastery level 1" )
    , ( "cornflowerDark", Colors.cornflowerDark, "Mastery level 1 text" )
    , ( "cornflowerLight", Colors.cornflowerLight, "Background to pair with Cornflower elements" )
    , ( "aqua", Colors.aqua, "Master level 2" )
    , ( "aquaDark", Colors.aquaDark, "Text to pair with Aqua elements" )
    , ( "aquaLight", Colors.aquaLight, "Background to pair with Aqua elements" )
    , ( "turquoise", Colors.turquoise, "Master level 3, writing cycles" )
    , ( "turquoiseDark", Colors.turquoiseDark, "Text to pair with turquoise elements" )
    , ( "turquoiseLight", Colors.turquoiseLight, "Background to pair with turquoise elements" )
    , ( "purple", Colors.purple, "Wrong, form errors, diagnostics, purple button" )
    , ( "purpleDark", Colors.purpleDark, "Purple text, purple button shadow" )
    , ( "purpleLight", Colors.purpleLight, "Purple backgrounds" )
    , ( "red", Colors.red, "NoRedInk red, form warnings, practice" )
    , ( "redDark", Colors.redDark, "Red links/text, red button shadow" )
    , ( "redLight", Colors.redLight, "Red backgrounds" )
    , ( "cyan", Colors.cyan, "Blue Highlighter" )
    , ( "magenta", Colors.magenta, "Pink highlighter" )
    , ( "mustard", Colors.mustard, "Diagnostic assignments, some Premium elements" )
    , ( "ochre", Colors.ochre, "Practice assignments background color, some Premium elements" )
    , ( "ochreDark", Colors.ochreDark, "Practice assignments text color" )
    , ( "sunshine", Colors.sunshine, "Yellow highlights, tips" )
    ]


backgroundHighlightColors : List ( String, Css.Color, String )
backgroundHighlightColors =
    [ ( "highlightYellow", Colors.highlightYellow, "Yellow background highlights" )
    , ( "highlightYellowDark", Colors.highlightYellowDark, "Dark yellow background highlights" )
    , ( "highlightCyan", Colors.highlightCyan, "Cyan background highlights" )
    , ( "highlightCyanDark", Colors.highlightCyanDark, "Dark cyan background highlights" )
    , ( "highlightMagenta", Colors.highlightMagenta, "Magenta background highlights" )
    , ( "highlightMagentaDark", Colors.highlightMagentaDark, "Dark magenta background highlights" )
    , ( "highlightGreen", Colors.highlightGreen, "Green background highlights" )
    , ( "highlightGreenDark", Colors.highlightGreenDark, "Dark green background highlights" )
    , ( "highlightBlue", Colors.highlightBlue, "Blue background highlights" )
    , ( "highlightBlueDark", Colors.highlightBlueDark, "Dark blue background highlights" )
    , ( "highlightPurple", Colors.highlightPurple, "Purple background highlights" )
    , ( "highlightPurpleDark", Colors.highlightPurpleDark, "Dark purple background highlights" )
    , ( "highlightBrown", Colors.highlightBrown, "Brown background highlights" )
    , ( "highlightBrownDark", Colors.highlightBrownDark, "Dark brown background highlights" )
    ]


textHighlightColors : List ( String, Css.Color, String )
textHighlightColors =
    [ ( "textHighlightYellow", Colors.textHighlightYellow, "Neutral text highlight #1" )
    , ( "textHighlightCyan", Colors.textHighlightCyan, "Neutral text highlight #2" )
    , ( "textHighlightMagenta", Colors.textHighlightMagenta, "Neutral text highlight #3" )
    , ( "textHighlightGreen", Colors.textHighlightGreen, "Neutral text highlight #4, Positive text highlight #1" )
    , ( "textHighlightBlue", Colors.textHighlightBlue, "Neutral text highlight #5, Positive text highlight #2" )
    , ( "textHighlightPurple", Colors.textHighlightPurple, "Negative text highlight #1" )
    , ( "textHighlightBrown", Colors.textHighlightBrown, "Negative text highlight #2" )
    ]


viewPreviewSwatch : ( String, Css.Color ) -> Html.Html msg
viewPreviewSwatch ( name, color ) =
    Html.div
        [ Attributes.css
            [ Css.textAlign Css.center
            , Css.padding2 (Css.px 8) Css.zero
            , Css.margin2 (Css.px 4) Css.zero
            , Css.borderRadius (Css.px 4)
            , Css.backgroundColor color
            , Css.color color
            , Css.fontSize (Css.px 14)
            ]
        ]
        [ Html.text name ]


viewColors : List ColorExample -> Html.Html msg
viewColors colors =
    colors
        |> List.map viewColor
        |> Html.div
            [ css
                [ Css.maxWidth (Css.px 12000)
                , Css.displayFlex
                , Css.flexWrap Css.wrap
                , Css.property "gap" "20px"
                , Css.margin3 (Css.px 10) Css.zero (Css.px 30)
                ]
            ]


viewColor : ColorExample -> Html.Html msg
viewColor ( name, color, description ) =
    Html.div
        [ css
            [ -- Dimensions
              Css.minWidth (Css.px 250)
            , Css.maxWidth (Css.px 250)
            , Css.minHeight (Css.px 160)
            , Css.maxHeight (Css.px 160)
            , Css.padding (Css.px 20)
            , Css.borderRadius (Css.px 20)

            -- Interior spacing
            , Css.displayFlex
            , Css.flexDirection Css.column
            , Css.justifyContent Css.center
            , Css.alignItems Css.center

            -- Colors
            , Css.backgroundColor color
            , Nri.Ui.Colors.Extra.fromCssColor color
                |> highContrast
                |> Nri.Ui.Colors.Extra.toCssColor
                |> Css.color
            ]
        ]
        [ Html.div
            [ css
                [ Css.fontSize (Css.px 20)
                , Css.fontWeight (Css.int 700)
                , Fonts.baseFont
                ]
            ]
            [ Html.text name ]
        , Html.div
            [ css
                [ Css.fontSize (Css.px 15)
                , Css.textAlign Css.center
                , Css.margin2 (Css.px 10) Css.zero
                , Fonts.baseFont
                ]
            ]
            [ Html.text description ]
        , Html.div
            [ css
                [ Css.fontSize (Css.px 20)
                , Fonts.baseFont
                ]
            ]
            [ Html.text color.value ]
        ]
