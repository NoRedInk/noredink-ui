module Nri.Ui.SegmentedControl.V10 exposing (Option, Width(..), view, viewSelect)

{-| Changes from V9:

  - hides non-displayed content rather than fully removing from the DOM, allowing for the segmented control to be used as a sticky-header that doesn't lose scroll position
  - :skull: removes NavConfig and SelectConfig
  - combines `view` and `viewSpa` (for V9 `view` behavior, be sure `toUrl` is Nothing. for V9 `viewSpa` behavior, pass through a Just as `toUrl`)
  - add custom attributes hole to the Option (in order to make SegmentedControls compatible with the Modal component)

@docs Option, Width, view, viewSpa, viewSelect

-}

import Accessibility.Styled exposing (..)
import Accessibility.Styled.Aria as Aria
import Accessibility.Styled.Role as Role
import Accessibility.Styled.Widget as Widget
import Css exposing (..)
import EventExtras
import Html.Styled
import Html.Styled.Attributes as Attributes exposing (css, href)
import Html.Styled.Events as Events
import Nri.Ui
import Nri.Ui.Colors.Extra exposing (withAlpha)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Svg.V1 as Svg exposing (Svg)
import Nri.Ui.Util exposing (dashify)


{-| -}
type alias Option a =
    { value : a
    , icon : Maybe Svg
    , label : String
    }


{-| -}
type Width
    = FitContent
    | FillContainer


{-| Creates _just the segmented select_ when you need the ui element itself and
not a page control

  - `onClick` : the message to produce when an option is selected (clicked) by the user
  - `options`: the list of options available
  - `selected`: if present, the value of the currently-selected option
  - `width`: how to size the segmented control

-}
viewSelect :
    { onClick : a -> msg
    , options : List (Option a)
    , selected : Maybe a
    , width : Width
    }
    -> Html msg
viewSelect config =
    div
        [ css
            [ displayFlex
            , cursor pointer
            ]
        , Role.radioGroup
        ]
        (List.map
            (viewSegment
                { onClick = config.onClick
                , selected = config.selected
                , width = config.width
                , selectedAttribute = Widget.selected True
                , ariaRole = Role.radio
                , toUrl = Nothing
                }
            )
            config.options
        )


{-|

  - `onClick` : the message to produce when an option is selected (clicked) by the user
  - `options`: the list of options available
  - `selected`: the value of the currently-selected option
  - `width`: how to size the segmented control
  - `content`: the panel content for the selected option
  - `toUrl`: a optional function that takes a `route` and returns the URL of that route. You should always use pass a `toUrl` function when the segmented control options correspond to routes in your SPA.

-}
view :
    { onClick : a -> msg
    , options : List (Option a)
    , selected : a
    , width : Width
    , content : Html msg
    , toUrl : Maybe (a -> String)
    }
    -> Html msg
view config =
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
                (viewSegment
                    { onClick = config.onClick
                    , selected = Just config.selected
                    , width = config.width
                    , toUrl = config.toUrl
                    , selectedAttribute = Aria.currentPage
                    , ariaRole = Role.tab
                    }
                )
                config.options
            )
        , tabPanel
            (List.filterMap identity
                [ Maybe.map (Aria.labelledBy << segmentIdFor) selected
                , Just <| css [ paddingTop (px 10) ]
                ]
            )
            [ config.content
            ]
        ]


segmentIdFor : Option a -> String
segmentIdFor option =
    "Nri-Ui-SegmentedControl-Segment-" ++ dashify option.label


panelIdFor : Option a -> String
panelIdFor option =
    "Nri-Ui-SegmentedControl-Panel-" ++ dashify option.label


viewSegment :
    { onClick : a -> msg
    , selected : Maybe a
    , width : Width
    , toUrl : Maybe (a -> String)
    , selectedAttribute : Attribute msg
    , ariaRole : Attribute msg
    }
    -> Option a
    -> Html msg
viewSegment config option =
    let
        idValue =
            segmentIdFor option

        element attrs children =
            case config.toUrl of
                Nothing ->
                    -- This is for a non-SPA view
                    button
                        (Events.onClick (config.onClick option.value)
                            :: attrs
                        )
                        children

                Just toUrl ->
                    -- This is a for a SPA view
                    Html.Styled.a
                        (href (toUrl option.value)
                            :: EventExtras.onClickPreventDefaultForLinkWithHref
                                (config.onClick option.value)
                            :: attrs
                        )
                        children
    in
    element
        (List.concat
            [ [ Attributes.id idValue
              , config.ariaRole
              , css sharedSegmentStyles
              ]
            , if Just option.value == config.selected then
                [ css focusedSegmentStyles
                , config.selectedAttribute
                ]

              else
                [ css unFocusedSegmentStyles ]
            , case config.width of
                FitContent ->
                    []

                FillContainer ->
                    [ css expandingTabStyles ]
            ]
        )
        [ case option.icon of
            Nothing ->
                text ""

            Just svg ->
                span
                    [ css
                        [ maxWidth (px 18)
                        , width (px 18)
                        , maxHeight (px 18)
                        , height (px 18)
                        , display inlineBlock
                        , verticalAlign textTop
                        , lineHeight (px 15)
                        , marginRight (px 8)
                        ]
                    ]
                    [ Svg.toHtml svg ]
        , text option.label
        ]


sharedSegmentStyles : List Style
sharedSegmentStyles =
    [ padding2 (px 6) (px 20)
    , height (px 45)
    , Fonts.baseFont
    , fontSize (px 15)
    , fontWeight bold
    , lineHeight (px 30)
    , margin zero
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


focusedSegmentStyles : List Style
focusedSegmentStyles =
    [ backgroundColor Colors.glacier
    , boxShadow5 inset zero (px 3) zero (withAlpha 0.2 Colors.gray20)
    , color Colors.navy
    ]


unFocusedSegmentStyles : List Style
unFocusedSegmentStyles =
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
