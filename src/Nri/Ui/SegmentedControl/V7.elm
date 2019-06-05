module Nri.Ui.SegmentedControl.V7 exposing (Config, Icon, Option, Width(..), view, viewSpa)

{-|

@docs Config, Icon, Option, Width, view, viewSpa

-}

import Accessibility.Styled exposing (..)
import Accessibility.Styled.Aria as Aria
import Accessibility.Styled.Role as Role
import Css exposing (..)
import EventExtras.Styled as EventExtras
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attr exposing (css, href)
import Html.Styled.Events as Events
import Nri.Ui
import Nri.Ui.Colors.Extra exposing (withAlpha)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Icon.V5 as Icon
import Nri.Ui.Util exposing (dashify)


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
        List.map (viewTab Nothing config) config.options


{-| Creates a segmented control that supports SPA navigation.
You should always use this instead of `view` when building a SPA
and the segmented control options correspond to routes in the SPA.

The first parameter is a function that takes a `route` and returns the URL of that route.

-}
viewSpa : (route -> String) -> Config route msg -> Html msg
viewSpa toUrl config =
    tabList <|
        List.map (viewTab (Just toUrl) config) config.options


tabList : List (Html.Html msg) -> Html.Html msg
tabList =
    Nri.Ui.styled div
        "Nri-Ui-SegmentedControl-tabList"
        [ displayFlex, cursor pointer ]
        [ Role.tabList ]


viewTab : Maybe (a -> String) -> Config a msg -> Option a -> Html.Html msg
viewTab maybeToUrl config option =
    let
        idValue =
            "Nri-Ui-SegmentedControl-" ++ dashify option.label

        element attrs children =
            case maybeToUrl of
                Nothing ->
                    -- This is for a non-SPA view
                    Html.button
                        (Events.onClick (config.onClick option.value)
                            :: attrs
                        )
                        children

                Just toUrl ->
                    -- This is a for a SPA view
                    Html.a
                        (href (toUrl option.value)
                            :: EventExtras.onClickPreventDefaultForLinkWithHref
                                (config.onClick option.value)
                            :: attrs
                        )
                        children
    in
    element
        (List.concat
            [ [ Attr.id idValue
              , Role.tab
              , css sharedTabStyles
              ]
            , if option.value == config.selected then
                [ css focusedTabStyles
                , Aria.currentPage
                ]

              else
                [ css unFocusedTabStyles ]
            , case config.width of
                FitContent ->
                    []

                FillContainer ->
                    [ css expandingTabStyles ]
            ]
        )
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
    , cursor pointer
    , property "transition" "background-color 0.2s, color 0.2s, box-shadow 0.2s, border 0.2s, border-width 0s"
    , textDecoration none
    ]


focusedTabStyles : List Style
focusedTabStyles =
    [ backgroundColor Colors.glacier
    , boxShadow5 inset zero (px 3) zero (withAlpha 0.2 Colors.gray20)
    , color Colors.navy
    ]


unFocusedTabStyles : List Style
unFocusedTabStyles =
    [ backgroundColor Colors.white
    , boxShadow5 inset zero (px -2) zero Colors.azure
    , color Colors.azure
    , hover [ backgroundColor Colors.glacier ]
    ]


expandingTabStyles : List Style
expandingTabStyles =
    [ flexGrow (int 1)
    , textAlign center
    ]
