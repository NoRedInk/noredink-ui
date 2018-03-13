module Nri.Ui.InputStyles exposing (Assets, CssClasses(..), styles)

{-|

@docs styles, CssClasses

-}

import Css exposing (..)
import Css.Foreign exposing (Snippet, adjacentSiblings, children, class, descendants, each, everything, media, selector, withClass)
import Nri.Accessibility
import Nri.Colors exposing (..)
import Nri.Fonts
import Nri.Stylers
import Nri.Ui.AssetPath as AssetPath exposing (Asset)
import Nri.Ui.CssFlexBoxWithVendorPrefix as FlexBox
import Nri.Ui.DatePickerConstants
import Nri.Ui.Styles.V1 as Styles


{-| Classes to be used in Nri Inputs such as Nri.TextInput and Nri.TextArea
-}
type CssClasses
    = Container
    | InvisibleLabel
    | Label
    | Input
    | IsInError
      -- For textarea
    | Writing
    | ContentCreation
      -- For date picker inputs
    | DatePickerContainer
    | TimePickerContainer
    | CalendarIcon
    | Date
      -- For searchbar
    | SearchBar
    | SearchFilterIcon
    | IsSearching


namespace : String
namespace =
    "Nri-Input-"


{-| -}
styles : Styles.StylesWithAssets Never CssClasses msg (Assets r)
styles =
    let
        focusedLabelSelector =
            -- This selects elements with the Label class that directly follows a focused input, so we can change the label styles on focus
            -- Should come out to: ".Nri-Input-Input:focus + .Nri-Input-Label"
            "."
                ++ namespace
                ++ toString Input
                ++ ":focus + "
                ++ "."
                ++ namespace
                ++ toString Label

        inputStyle =
            [ border3 (px 1) solid gray75
            , borderRadius (px 8)
            , padding2 (px 8) (px 14)
            , property "transition" "all 0.1s ease"
            , pseudoClass "placeholder"
                [ color gray45
                ]
            , Nri.Stylers.makeFont (px 15) gray20

            -- fix bootstrap
            , display inlineBlock
            , verticalAlign top
            , marginBottom zero
            , lineHeight (px 20)
            , marginTop (px 9)
            , boxShadow6 inset zero (px 2) zero zero Nri.Colors.gray92
            , property "transition" "all 0.4s ease"
            , focus
                [ borderColor azure
                , outline none
                , boxShadow6 inset zero (px 2) zero zero Nri.Colors.glacier
                ]
            ]
    in
    Styles.stylesWithAssets namespace <|
        \assets ->
            [ selector "input"
                [ withClass Input
                    (inputStyle
                        ++ [ height (px 45)
                           , width (pct 100)
                           ]
                    )
                ]
            , selector "textarea"
                [ withClass Input
                    (inputStyle
                        ++ [ height (px 100)
                           , width (pct 100)
                           ]
                    )
                ]
            , class Container
                [ position relative
                ]
            , class InvisibleLabel
                [ Nri.Accessibility.invisibleText
                ]
            , class Label
                [ backgroundColor Nri.Colors.white
                , left (px 10)
                , top (px 0)
                , padding2 zero (px 5)
                , Nri.Stylers.makeFont (px 12) navy
                , position absolute
                , fontWeight (int 600)
                , property "transition" "all 0.4s ease"
                ]
            , class Writing
                [ descendants
                    [ class Input
                        [ Nri.Fonts.quizFont
                        , fontSize (px 20)
                        , lineHeight (px 25)
                        , minHeight (px 150)
                        , padding (px 15)
                        , paddingTop (px 20)
                        ]
                    , class Label
                        [ border3 (px 1) solid Nri.Colors.gray75
                        , borderRadius (px 4)
                        , Nri.Stylers.makeFont (px 15) Nri.Colors.navy
                        ]
                    , selector focusedLabelSelector
                        [ backgroundColor Nri.Colors.azure
                        , color Nri.Colors.white
                        , borderColor Nri.Colors.azure
                        ]
                    ]
                , withClass IsInError
                    [ descendants
                        [ class Label
                            [ color Nri.Colors.purple
                            , backgroundColor Nri.Colors.white
                            , borderColor Nri.Colors.purple
                            ]
                        , selector focusedLabelSelector
                            [ backgroundColor Nri.Colors.purple
                            , color Nri.Colors.white
                            , borderColor Nri.Colors.purple
                            ]
                        ]
                    ]
                ]
            , class ContentCreation
                [ descendants
                    [ class Label
                        [ border3 (px 1) solid Nri.Colors.gray75
                        , borderRadius (px 4)
                        , Nri.Stylers.makeFont (Css.px 11) Nri.Colors.gray45
                        , padding2 (px 2) (px 5)
                        ]
                    ]
                ]
            , class IsInError
                [ descendants
                    [ class Label [ color purple ]
                    , class Input
                        [ borderColor purple
                        , boxShadow6 inset zero (px 2) zero zero Nri.Colors.purpleLight
                        , focus
                            [ borderColor purple
                            , boxShadow6 inset zero (px 2) zero zero Nri.Colors.purpleLight
                            ]
                        ]
                    ]
                ]
            , class DatePickerContainer
                [ position relative
                , FlexBox.flexGrow 3
                , FlexBox.flexBasis (px 128)
                ]
            , class TimePickerContainer
                [ marginLeft (px 3)
                , position relative
                , FlexBox.flexGrow 1
                , FlexBox.flexBasis (px 91)
                ]
            , class CalendarIcon
                -- Used for date picker
                [ position absolute
                , backgroundImage (url <| AssetPath.url assets.iconCalendar_svg)
                , backgroundRepeat noRepeat
                , height (px 25)
                , width (px 25)
                , top (px 18)
                , right (px 5)
                , cursor pointer
                , property "pointer-events" "none"
                ]
            , class Date
                [ FlexBox.flexGrow 1
                , position relative
                , paddingBottom (px 5)
                , FlexBox.displayFlex
                ]
            , selector Nri.Ui.DatePickerConstants.dialogTag
                [ backgroundColor Nri.Colors.gray96
                ]
            , selector Nri.Ui.DatePickerConstants.datePickerTag
                [ property "min-width" "auto" ]
            , class SearchBar
                [ descendants
                    [ class Input
                        [ margin zero ]
                    ]
                ]
            , class SearchFilterIcon
                [ position absolute
                , backgroundImage (url <| AssetPath.url assets.icons_searchGray_svg)
                , backgroundRepeat noRepeat
                , backgroundSize contain
                , height (px 20)
                , width (px 20)
                , top (px 12)
                , right (px 10)
                , withClass IsSearching
                    [ backgroundImage (url <| AssetPath.url assets.icons_xBlue_svg)
                    , cursor pointer
                    , Css.height (px 14)
                    , Css.width (px 14)
                    , top (px 16)
                    ]
                ]
            ]


type alias Assets r =
    { r
        | iconCalendar_svg : Asset
        , icons_searchGray_svg : Asset
        , icons_xBlue_svg : Asset
    }
