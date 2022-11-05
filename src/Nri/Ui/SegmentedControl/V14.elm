module Nri.Ui.SegmentedControl.V14 exposing
    ( Option, view
    , Radio, viewRadioGroup
    , Positioning(..), Width(..)
    )

{-| Patch changes:

  - use Tooltip.V3 instead of Tooltip.V2
  - when tooltips aren't used, avoid using aria-owns and rendering tooltip-related code

Changes from V13:

  - Adds tooltip support to `viewRadioGroup`

@docs Option, view
@docs Radio, viewRadioGroup
@docs Positioning, Width

-}

import Accessibility.Styled exposing (..)
import Accessibility.Styled.Aria as Aria
import Accessibility.Styled.Role as Role
import Accessibility.Styled.Style as Style
import Css exposing (..)
import Html.Styled
import Html.Styled.Attributes as Attributes exposing (css)
import Html.Styled.Events as Events
import Nri.Ui.Colors.Extra as ColorsExtra
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.FocusRing.V1 as FocusRing
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Svg.V1 as Svg exposing (Svg)
import Nri.Ui.Tooltip.V3 as Tooltip
import Nri.Ui.Util exposing (safeId)
import TabsInternal.V2 as TabsInternal


{-| -}
type Positioning
    = Left Width
    | Center


{-| -}
type Width
    = FitContent
    | FillContainer


{-| -}
type alias Radio value msg =
    { value : value
    , idString : String
    , label : Html msg
    , attributes : List (Attribute msg)
    , tooltip : List (Tooltip.Attribute msg)
    , icon : Maybe Svg
    }


{-| Creates a set of radio buttons styled to look like a segmented control.

  - `onSelect`: the message to produce when an option is selected (clicked) by the user
  - `idString`: function to get the radio value as a string
  - `options`: the list of options available
  - `selected`: if present, the value of the currently-selected option
  - `positioning`: how to position and size the segmented control
  - `legend`:
      - value read to screenreader users to explain the radio group's purpose <https://dequeuniversity.com/rules/axe/3.3/radiogroup?application=axeAPI>
      - after lowercasing & dashifying, this value is used to group the radio buttons together

-}
viewRadioGroup :
    { onSelect : a -> msg
    , options : List (Radio a msg)
    , selected : Maybe a
    , positioning : Positioning
    , legend : String
    }
    -> Html msg
viewRadioGroup config =
    let
        numOptions =
            List.length config.options

        viewRadio index option =
            let
                isSelected =
                    Just option.value == config.selected

                inner : List (Attribute msg) -> Html msg
                inner extraAttrs =
                    Html.Styled.label
                        (css
                            (styles "focus-within" config.positioning numOptions index isSelected)
                            :: Attributes.class FocusRing.customClass
                            :: Attributes.class "segmented-control-radio-element"
                            :: extraAttrs
                        )
                        [ radio name option.idString isSelected <|
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
                        , div [] [ viewIcon option.icon, option.label ]
                        ]
            in
            case option.tooltip of
                [] ->
                    inner []

                _ ->
                    Tooltip.view
                        { id = option.idString ++ "-tooltip"
                        , trigger = inner
                        }
                        (case config.positioning of
                            Left FillContainer ->
                                Tooltip.containerCss [ Css.width (Css.pct 100) ] :: option.tooltip

                            _ ->
                                option.tooltip
                        )

        name =
            safeId config.legend

        legendId =
            "legend-" ++ name
    in
    div
        [ Role.radioGroup
        , Aria.labelledBy legendId
        , css
            [ displayFlex
            , cursor pointer
            , case config.positioning of
                Left _ ->
                    justifyContent flexStart

                Center ->
                    justifyContent center
            ]
        ]
        (p (Attributes.id legendId :: Style.invisible) [ text config.legend ]
            :: List.indexedMap viewRadio config.options
        )


{-| Tooltip defaults: `[Tooltip.smallPadding, Tooltip.onBottom, Tooltip.fitToContent]`
-}
type alias Option value msg =
    { value : value
    , idString : String
    , label : Html msg
    , attributes : List (Attribute msg)
    , tabTooltip : List (Tooltip.Attribute msg)
    , icon : Maybe Svg
    , content : Html msg
    }


{-|

  - `focusAndSelect` : the message to produce when an option is selected by the user
  - `options`: the list of options available
  - `selected`: the value of the currently-selected option
  - `positioning`: how to position and size the segmented control
  - `toUrl`: a optional function that takes a `route` and returns the URL of that route. You should always use pass a `toUrl` function when the segmented control options correspond to routes in your SPA.

-}
view :
    { focusAndSelect : { select : a, focus : Maybe String } -> msg
    , options : List (Option a msg)
    , selected : a
    , positioning : Positioning
    , toUrl : Maybe (a -> String)
    }
    -> Html msg
view config =
    let
        toInternalTab : Option a msg -> TabsInternal.Tab a msg
        toInternalTab option =
            { id = option.value
            , idString = option.idString
            , tabAttributes = Attributes.class FocusRing.customClass :: option.attributes
            , tabTooltip =
                case config.positioning of
                    Left FillContainer ->
                        Tooltip.containerCss [ Css.width (Css.pct 100) ] :: option.tabTooltip

                    _ ->
                        option.tabTooltip
            , tabView = [ viewIcon option.icon, option.label ]
            , panelView = option.content
            , spaHref = Maybe.map (\toUrl -> toUrl option.value) config.toUrl
            , disabled = False
            , labelledBy = Nothing
            , describedBy = []
            }

        { tabList, tabPanels } =
            TabsInternal.views
                { focusAndSelect = config.focusAndSelect
                , selected = config.selected
                , tabs = List.map toInternalTab config.options
                , tabListStyles =
                    [ displayFlex
                    , cursor pointer
                    , marginBottom (px 10)
                    , case config.positioning of
                        Left _ ->
                            justifyContent flexStart

                        Center ->
                            justifyContent center
                    ]
                , tabStyles = styles "focus-visible" config.positioning (List.length config.options)
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


styles : String -> Positioning -> Int -> Int -> Bool -> List Style
styles focusSelector positioning numEntries index isSelected =
    [ sharedSegmentStyles numEntries index
    , if isSelected then
        focusedSegmentStyles

      else
        unFocusedSegmentStyles
    , Css.batch <|
        case positioning of
            Left FillContainer ->
                [ width (Css.pct 100)
                , flexGrow (int 1)
                , textAlign center
                ]

            _ ->
                []
    , -- ensure that the focus state is visible & looks nice
      Css.pseudoClass focusSelector
        [ FocusRing.boxShadows [ focusedSegmentBoxShadowValue ]
        , Css.outline3 (Css.px 2) Css.solid Css.transparent
        , outlineStyle solid
        , outlineWidth (Css.px 1)
        , zIndex (int 1)
        ]
    ]


sharedSegmentStyles : Int -> Int -> Style
sharedSegmentStyles numEntries index =
    [ padding2 (px 6) (px 15)
    , height (px 45)
    , Fonts.baseFont
    , fontSize (px 15)
    , fontWeight bold
    , lineHeight (px 30)
    , margin zero
    , border3 (px 1) solid Colors.azure
    , boxSizing borderBox
    , cursor pointer
    , property "transition" "background-color 0.2s, color 0.2s, box-shadow 0.2s, border 0.2s, border-width 0s"
    , textDecoration none
    , hover [ textDecoration none ]
    , focus [ textDecoration none ]
    ]
        ++ (if index == 0 then
                [ borderTopLeftRadius (px 8)
                , borderBottomLeftRadius (px 8)
                ]

            else if index == numEntries - 1 then
                [ borderTopRightRadius (px 8)
                , borderBottomRightRadius (px 8)
                , borderLeft (px 0)
                ]

            else
                [ borderLeft (px 0) ]
           )
        |> Css.batch


focusedSegmentStyles : Style
focusedSegmentStyles =
    [ backgroundColor Colors.glacier
    , Css.property "box-shadow" focusedSegmentBoxShadowValue
    , color Colors.navy
    ]
        |> Css.batch


focusedSegmentBoxShadowValue : String
focusedSegmentBoxShadowValue =
    let
        colorStr =
            ColorsExtra.withAlpha 0.2 Colors.gray20
                |> ColorsExtra.toCssString
    in
    "inset 0 3px 0 " ++ colorStr


unFocusedSegmentStyles : Style
unFocusedSegmentStyles =
    [ backgroundColor Colors.white
    , boxShadow5 inset zero (px -2) zero Colors.azure
    , color Colors.azure
    , hover [ backgroundColor Colors.frost ]
    ]
        |> Css.batch
