module Nri.Ui.SegmentedControl.V12 exposing
    ( Option, view
    , Radio, viewRadioGroup
    , Width(..)
    )

{-| Changes from V11:

  - allow HTML in labels

@docs Option, view
@docs Radio, viewRadioGroup
@docs Width

-}

import Accessibility.Styled exposing (..)
import Accessibility.Styled.Aria as Aria
import Accessibility.Styled.Role as Role
import Accessibility.Styled.Style as Style
import Accessibility.Styled.Widget as Widget
import Css exposing (..)
import EventExtras
import Html.Styled
import Html.Styled.Attributes as Attributes exposing (css, href)
import Html.Styled.Events as Events
import Json.Encode as Encode
import Nri.Ui
import Nri.Ui.Colors.Extra exposing (withAlpha)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Html.Attributes.V2 as AttributesExtra
import Nri.Ui.Svg.V1 as Svg exposing (Svg)
import Nri.Ui.Util exposing (dashify)
import TabsInternal


{-| -}
type Width
    = FitContent
    | FillContainer


{-| -}
type alias Radio value msg =
    { value : value
    , label : String
    , attributes : List (Attribute msg)
    , icon : Maybe Svg
    }


{-| Creates a set of radio buttons styled to look like a segmented control.

  - `onSelect`: the message to produce when an option is selected (clicked) by the user
  - `toString`: function to get the radio value as a string
  - `options`: the list of options available
  - `selected`: if present, the value of the currently-selected option
  - `width`: how to size the segmented control
  - `legend`:
      - value read to screenreader users to explain the radio group's purpose <https://dequeuniversity.com/rules/axe/3.3/radiogroup?application=axeAPI>
      - after lowercasing & dashifying, this value is used to group the radio buttons together

-}
viewRadioGroup :
    { onSelect : a -> msg
    , toString : a -> String
    , options : List (Radio a msg)
    , selected : Maybe a
    , width : Width
    , legend : String
    }
    -> Html msg
viewRadioGroup config =
    let
        viewRadio option =
            let
                isSelected =
                    Just option.value == config.selected
            in
            labelAfter
                [ css
                    -- ensure that the focus state is visible, even
                    -- though the radio button that technically has focus
                    -- is not
                    (Css.pseudoClass "focus-within"
                        [ Css.property "outline-style" "auto" ]
                        :: styles config.width isSelected
                    )
                ]
                (div [] [ viewIcon option.icon, text option.label ])
                (radio name (config.toString option.value) isSelected <|
                    (Events.onCheck (\_ -> config.onSelect option.value)
                        :: css [ Css.opacity Css.zero ]
                        :: Attributes.attribute "data-nri-checked"
                            (if isSelected then
                                "true"

                             else
                                "false"
                            )
                        :: Style.invisible
                    )
                )

        name =
            dashify (String.toLower config.legend)

        legendId =
            "legend-" ++ name
    in
    div
        [ Role.radioGroup
        , Aria.labelledBy legendId
        , css [ displayFlex, cursor pointer ]
        ]
        (p (Attributes.id legendId :: Style.invisible) [ text config.legend ]
            :: List.map viewRadio config.options
        )


{-| -}
type alias Option value msg =
    { value : value
    , label : Html msg
    , attributes : List (Attribute msg)
    , icon : Maybe Svg
    , content : Html msg
    }


{-|

  - `onSelect` : the message to produce when an option is selected by the user
  - `onFocus` : the message to focus an element by id string
  - `toString` : function to get the option value as a string
  - `options`: the list of options available
  - `selected`: the value of the currently-selected option
  - `width`: how to size the segmented control
  - `toUrl`: a optional function that takes a `route` and returns the URL of that route. You should always use pass a `toUrl` function when the segmented control options correspond to routes in your SPA.

-}
view :
    { onSelect : a -> msg
    , onFocus : String -> msg
    , toString : a -> String
    , options : List (Option a msg)
    , selected : a
    , width : Width
    , toUrl : Maybe (a -> String)
    }
    -> Html msg
view config =
    let
        toInternalTab : Option a msg -> TabsInternal.Tab a msg
        toInternalTab option =
            { id = option.value
            , idString = config.toString option.value
            , tabAttributes = option.attributes
            , tabView = [ viewIcon option.icon, option.label ]
            , panelView = option.content
            , spaHref = Maybe.map (\toUrl -> toUrl option.value) config.toUrl
            }

        { tabList, tabPanels } =
            TabsInternal.views
                { onSelect = config.onSelect
                , onFocus = config.onFocus
                , selected = config.selected
                , tabs = List.map toInternalTab config.options
                , tabListStyles = [ displayFlex, cursor pointer, marginBottom (px 10) ]
                , tabStyles = styles config.width
                }
    in
    div []
        [ tabList
        , tabPanels
        ]


viewIcon : Maybe Svg.Svg -> Html msg
viewIcon icon =
    case icon of
        Nothing ->
            text ""

        Just svg ->
            svg
                |> Svg.withWidth (px 18)
                |> Svg.withHeight (px 18)
                |> Svg.withCss
                    [ display inlineBlock
                    , verticalAlign textTop
                    , lineHeight (px 15)
                    , marginRight (px 8)
                    ]
                |> Svg.toHtml


styles : Width -> Bool -> List Style
styles width isSelected =
    [ sharedSegmentStyles
    , if isSelected then
        focusedSegmentStyles

      else
        unFocusedSegmentStyles
    , case width of
        FitContent ->
            Css.batch []

        FillContainer ->
            expandingTabStyles
    ]


sharedSegmentStyles : Style
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
    , hover [ textDecoration none ]
    , focus [ textDecoration none ]
    ]
        |> Css.batch


focusedSegmentStyles : Style
focusedSegmentStyles =
    [ backgroundColor Colors.glacier
    , boxShadow5 inset zero (px 3) zero (withAlpha 0.2 Colors.gray20)
    , color Colors.navy
    ]
        |> Css.batch


unFocusedSegmentStyles : Style
unFocusedSegmentStyles =
    [ backgroundColor Colors.white
    , boxShadow5 inset zero (px -2) zero Colors.azure
    , color Colors.azure
    , hover [ backgroundColor Colors.frost ]
    ]
        |> Css.batch


expandingTabStyles : Style
expandingTabStyles =
    [ flexGrow (int 1)
    , textAlign center
    ]
        |> Css.batch
