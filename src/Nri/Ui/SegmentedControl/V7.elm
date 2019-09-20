module Nri.Ui.SegmentedControl.V7 exposing (Config, Icon, Option, Width(..), view, viewSpa, ToggleConfig, viewToggle)

{-|

@docs Config, Icon, Option, Width, view, viewSpa, ToggleConfig, viewToggle

-}

import Accessibility.Styled exposing (..)
import Accessibility.Styled.Aria as Aria
import Accessibility.Styled.Role as Role
import Accessibility.Styled.Widget as Widget
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


{-|

  - `onClick` : the message to produce when an option is selected (clicked) by the user
  - `options`: the list of options available
  - `selected`: the value of the currently-selected option
  - `width`: how to size the segmented control
  - `content`: the panel content for the selected option

-}
type alias Config a msg =
    { onClick : a -> msg
    , options : List (Option a)
    , selected : a
    , width : Width
    , content : Html msg
    }


{-| Same shape as Config but without the content
-}
type alias ToggleConfig a msg =
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
    viewHelper Nothing config


{-| Creates a segmented control that supports SPA navigation.
You should always use this instead of `view` when building a SPA
and the segmented control options correspond to routes in the SPA.

The first parameter is a function that takes a `route` and returns the URL of that route.

-}
viewSpa : (route -> String) -> Config route msg -> Html msg
viewSpa toUrl config =
    viewHelper (Just toUrl) config


{-| Creates _just the toggle_ when need the ui element itself and not a page control
-}
viewToggle : ToggleConfig a msg -> Html.Html msg
viewToggle config =
    tabList
        [ css
            [ displayFlex
            , cursor pointer
            ]
        ]
        (List.map
            (viewTab
                { onClick = config.onClick
                , selected = config.selected
                , width = config.width
                , selectedAttribute = Widget.selected True
                , maybeToUrl = Nothing
                }
            )
            config.options
        )


viewHelper : Maybe (a -> String) -> Config a msg -> Html msg
viewHelper maybeToUrl config =
    let
        selected =
            config.options
                |> List.filter (\o -> o.value == config.selected)
                |> List.head
    in
    div []
        [ tabList
            [ css
                [ displayFlex
                , cursor pointer
                ]
            ]
            (List.map
                (viewTab
                    { onClick = config.onClick
                    , selected = config.selected
                    , width = config.width
                    , selectedAttribute = Aria.currentPage
                    , maybeToUrl = maybeToUrl
                    }
                )
                config.options
            )
        , tabPanel
            (List.filterMap identity
                [ Maybe.map (Aria.labelledBy << tabIdFor) selected
                , Just <| css [ paddingTop (px 10) ]
                ]
            )
            [ config.content
            ]
        ]


tabIdFor : Option a -> String
tabIdFor option =
    "Nri-Ui-SegmentedControl-Tab-" ++ dashify option.label


panelIdFor : Option a -> String
panelIdFor option =
    "Nri-Ui-SegmentedControl-Panel-" ++ dashify option.label


viewTab :
    { onClick : a -> msg
    , selected : a
    , width : Width
    , selectedAttribute : Html.Attribute msg
    , maybeToUrl : Maybe (a -> String)
    }
    -> Option a
    -> Html.Html msg
viewTab config option =
    let
        idValue =
            tabIdFor option

        element attrs children =
            case config.maybeToUrl of
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
                , config.selectedAttribute
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
    , hover
        [ textDecoration none
        ]
    , focus
        [ textDecoration none
        ]
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
    , hover
        [ backgroundColor Colors.frost
        ]
    ]


expandingTabStyles : List Style
expandingTabStyles =
    [ flexGrow (int 1)
    , textAlign center
    ]
