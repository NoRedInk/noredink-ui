module Examples.Colors exposing (example)

{-|

@docs example

-}

import Color exposing (highContrast)
import Css
import Html.Styled as Html
import Html.Styled.Attributes as Attributes exposing (css)
import ModuleExample as ModuleExample exposing (Category(..), ModuleExample)
import Nri.Ui.Colors.Extra
import Nri.Ui.Colors.V1 as Colors


example : ModuleExample msg
example =
    { name = "Nri.Ui.Colors.V1"
    , category = Colors
    , content =
        [ [ ( "gray20", Colors.gray20, "Main text" )
          , ( "gray45", Colors.gray45, "Secondary text, 0-69 score" )
          , ( "gray75", Colors.gray75, "Border of form elements and tabs" )
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
          , ( "yellow", Colors.yellow, "Yellow highlighter" )
          , ( "ochre", Colors.ochre, "Yellow button shadow" )
          , ( "sunshine", Colors.sunshine, "Yellow highlights, tips" )
          , ( "highlightYellow", Colors.highlightYellow, "Yellow background highlights" )
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
            |> List.map viewColor
            |> Html.div
                [ css
                    [ Css.maxWidth (Css.px 12000)
                    , Css.displayFlex
                    , Css.flexWrap Css.wrap
                    ]
                ]
        ]
    }


viewColor : ( String, Css.Color, String ) -> Html.Html msg
viewColor ( name, color, description ) =
    Html.div
        [ css
            [ -- Dimensions
              Css.minWidth (Css.px 250)
            , Css.maxWidth (Css.px 250)
            , Css.minHeight (Css.px 160)
            , Css.maxHeight (Css.px 160)
            , Css.margin (Css.px 4)
            , Css.padding (Css.px 8)
            , Css.borderRadius (Css.px 4)

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
            [ css [ Css.fontSize (Css.px 20) ]
            ]
            [ Html.text name ]
        , Html.div
            [ css
                [ Css.fontSize (Css.px 10)
                , Css.flexGrow (Css.num 1)
                , Css.textAlign Css.center
                ]
            ]
            [ Html.text description ]
        , Html.div
            [ css
                [ Css.fontSize (Css.px 20)
                , Css.flexGrow (Css.num 2)
                ]
            ]
            [ Html.text color.value ]
        ]
