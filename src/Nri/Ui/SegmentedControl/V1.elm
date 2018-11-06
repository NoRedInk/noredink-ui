module Nri.Ui.SegmentedControl.V1 exposing (Config, Option, styles, view)

{-|

@docs Config, Option, styles, view

-}

import Accessibility exposing (..)
import Accessibility.Role as Role
import Css exposing (..)
import Css.Foreign exposing (Snippet, adjacentSiblings, children, class, descendants, each, everything, media, selector, withClass)
import DEPRECATED.Nri.Ui.Styles.V1
import Html
import Html.Attributes
import Html.Events
import Nri.Ui.Colors.Extra exposing (withAlpha)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.CssFlexBoxWithVendorPrefix as FlexBox
import Nri.Ui.Fonts.V1


{-| -}
type alias Config a msg =
    { onClick : a -> msg
    , options : List (Option a)
    , selected : a
    }


{-| -}
type alias Option a =
    { value : a
    , label : String
    , id : String
    }


{-| -}
view : Config a msg -> Html.Html msg
view config =
    config.options
        |> List.map
            (\option ->
                Html.div
                    [ Html.Attributes.id option.id
                    , Role.tab
                    , Html.Events.onClick (config.onClick option.value)
                    , styles.classList
                        [ ( Tab, True )
                        , ( Focused, option.value == config.selected )
                        ]
                    ]
                    [ Html.text option.label
                    ]
            )
        |> div [ Role.tabList, styles.class [ SegmentedControl ] ]


type CssClass
    = SegmentedControl
    | Tab
    | Focused


{-| -}
styles : DEPRECATED.Nri.Ui.Styles.V1.Styles Never CssClass msg
styles =
    DEPRECATED.Nri.Ui.Styles.V1.styles "Nri-Ui-SegmentedControl-V1-"
        [ Css.Foreign.class SegmentedControl
            [ FlexBox.displayFlex
            , cursor pointer
            ]
        , Css.Foreign.class Tab
            [ padding2 (px 6) (px 20)
            , height (px 45)
            , Nri.Ui.Fonts.V1.baseFont
            , fontSize (px 15)
            , color Colors.azure
            , fontWeight bold
            , lineHeight (px 30)
            , firstChild
                [ borderTopLeftRadius (px 8)
                , borderBottomLeftRadius (px 8)
                , borderLeft3 (px 1) solid Colors.azure
                ]
            , lastChild
                [ borderTopRightRadius (px 8)
                , borderBottomRightRadius (px 8)
                ]
            , border3 (px 1) solid Colors.azure
            , borderLeft (px 0)
            , borderBottom3 (px 3) solid Colors.azure
            , boxSizing borderBox
            ]
        , Css.Foreign.class Focused
            [ color Colors.gray20
            , backgroundColor Colors.glacier
            , boxShadow5 inset zero (px 3) zero (withAlpha 0.2 Colors.gray20)
            , borderBottom3 (px 1) solid Colors.azure
            ]
        ]
