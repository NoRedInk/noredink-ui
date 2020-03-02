module Nri.Ui.SegmentedControl.V6 exposing (Config, Icon, Option, Width(..), view)

{-|

@docs Config, Icon, Option, Width, view

-}

import Accessibility.Styled exposing (..)
import Accessibility.Styled.Role as Role
import Css exposing (..)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attr exposing (css)
import Html.Styled.Events as Events
import Nri.Ui
import Nri.Ui.Colors.Extra exposing (withAlpha)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Icon.V3 as Icon


{-| -}
type alias Config a msg =
    { onClick : a -> msg
    , options : List (Option a)
    , selected : a
    , width : Width
    }


{-| -}
type alias Option a =
    { value : a
    , icon : Maybe Icon
    , label : String
    , id : String
    }


{-| -}
type Width
    = FitContent
    | FillContainer


{-| -}
type alias Icon =
    { alt : String
    , icon : Icon.IconType
    }


{-| -}
view : Config a msg -> Html.Html msg
view config =
    tabList <|
        List.map (viewTab config) config.options


tabList : List (Html.Html msg) -> Html.Html msg
tabList =
    Nri.Ui.styled div
        "Nri-Ui-SegmentedControl-tabList"
        [ displayFlex, cursor pointer ]
        [ Role.tabList ]


viewTab : Config a msg -> Option a -> Html.Html msg
viewTab config option =
    Html.div
        [ Attr.id option.id
        , Role.tab
        , Events.onClick (config.onClick option.value)
        , css sharedTabStyles
        , css <|
            if option.value == config.selected then
                focusedTabStyles

            else
                unFocusedTabStyles
        , css <|
            case config.width of
                FitContent ->
                    []

                FillContainer ->
                    expandingTabStyles
        ]
        [ case option.icon of
            Nothing ->
                Html.text ""

            Just icon ->
                viewIcon icon
        , Html.text option.label
        ]


viewIcon : Icon -> Html.Html msg
viewIcon icon =
    Html.span
        [ css [ marginRight (px 10) ] ]
        [ Icon.icon icon ]


sharedTabStyles : List Style
sharedTabStyles =
    [ padding2 (px 6) (px 20)
    , height (px 45)
    , Fonts.baseFont
    , fontSize (px 15)
    , color Colors.azure
    , fontWeight bold
    , lineHeight (px 30)
    , firstOfType
        [ borderTopLeftRadius (px 8)
        , borderBottomLeftRadius (px 8)
        , borderLeft3 (px 1) solid Colors.azure
        ]
    , lastOfType
        [ borderTopRightRadius (px 8)
        , borderBottomRightRadius (px 8)
        ]
    , border3 (px 1) solid Colors.azure
    , borderLeft (px 0)
    , boxSizing borderBox
    ]


focusedTabStyles : List Style
focusedTabStyles =
    [ backgroundColor Colors.glacier
    , boxShadow5 inset zero (px 3) zero (withAlpha 0.2 Colors.gray20)
    , color Colors.gray20
    ]


unFocusedTabStyles : List Style
unFocusedTabStyles =
    [ backgroundColor Colors.white
    , boxShadow5 inset zero (px -2) zero Colors.azure
    , color Colors.azure
    ]


expandingTabStyles : List Style
expandingTabStyles =
    [ flexGrow (int 1)
    , textAlign center
    ]
